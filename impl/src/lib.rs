//
// Copyright (c) Zach Marcantel. All rights reserved.
// Licensed under the GPLv3. See LICENSE file in the project root
// for full license information.
//

extern crate proc_macro;

#[macro_use]
extern crate quote;


/// Wrapper type for the visibility of the generated struct
/// and the parsed syntax defining the regions.
struct BitRegions {
    vis: Option<syn::token::Pub>,
    struct_def: Struct,
    user_fns: UserFns,
}

impl syn::parse::Parse for BitRegions {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let vis = if input.peek(syn::token::Pub) {
            Some(input.parse()?)
        } else {
            None
        };

        let struct_def: Struct = input.parse()?;
        let user_fns: UserFns = input.parse()?;

        Ok(BitRegions{
            vis: vis,
            struct_def,
            user_fns,
        })
    }
}

/// The memory location to default initialize the region to.
enum MemoryLocation {
    Lit(syn::LitInt),
    Ident(syn::Ident),
    Expr(syn::ExprBinary),
}

impl syn::parse::Parse for MemoryLocation {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        if !input.peek2(syn::token::Brace) {
            return Ok(MemoryLocation::Expr(input.parse()?));
        }

        if input.peek(syn::Ident) {
            return Ok(MemoryLocation::Ident(input.parse()?));
        }
        if input.peek(syn::LitInt) {
            return Ok(MemoryLocation::Lit(input.parse()?));
        }

        Err(syn::Error::new(
            input.span(),
            "expected ident, literal, or const expression",
        ))
    }
}

/// Holds the identitiy, numeric type representation, and regions.
struct Struct {
    ident: syn::Ident,
    repr: syn::Type,
    default_loc: Option<MemoryLocation>,
    fields: syn::punctuated::Punctuated<Field, syn::Token![,]>,
}

impl syn::parse::Parse for Struct {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        // grab the identity and "C" representation
        let ident: syn::Ident = input.parse()?;
        let repr: syn::Type = input.parse()?;

        let mut default_loc = None;
        if input.peek(syn::token::At) {
            let _: syn::token::At = input.parse()?;
            default_loc = Some(input.parse()?);
        }

        // extract everything wrapped in the braces
        // also collect a region per parsed field
        let content;
        let _: syn::token::Brace = syn::braced!(content in input);
        let fields = content.parse_terminated(Field::parse)?;
        let regions = fields.iter().map(|f| f.region.clone())
            .collect::<Vec<Region>>();

        // check for intersections
        for (i, r) in regions.iter().enumerate() {
            // check all entries we haven't been checked against
            for k in (i+1)..regions.len() {
                let other = &regions[k];

                if r.intersects(other) {
                    let oth_err = syn::Error::new(
                        other.lit.span(), "other region");
                    let mut err = syn::Error::new(
                        r.lit.span(),
                        format!("0b{:b} intersected by other region 0b{:b}",
                            r.value, other.value),
                    );
                    err.combine(oth_err);
                    return Err(err);
                }
            }
        }

        Ok(Struct {
            ident,
            repr,
            default_loc,
            fields,
        })
    }
}


/// Syntax representation of a region with a name and bit-region.
struct Field {
    name: syn::Ident,
    lower_name: syn::Ident,
    region: Region,
}
impl Field {
    /// Generate the operations on this field.
    pub fn gen_ops(&self, struct_name: &syn::Ident, repr: &syn::Type)
        -> quote::__rt::TokenStream
    {
        if self.region.len() == 1 {
            self.gen_single_bit_ops(struct_name)
        } else {
            self.gen_region_ops(struct_name, repr)
        }
    }

    /// Generate the "with_{name}" constructor
    pub fn gen_with_ctor(&self, struct_name: &syn::Ident, repr: &syn::Type)
        -> quote::__rt::TokenStream
    {
        let name = &self.name;
        let lower = &self.lower_name;
        let with_fn = format_ident!("with_{}", lower);
        let set_call = format_ident!("set_{}", lower);
        if self.region.len() == 1 {
            quote! {
                pub fn #with_fn() -> #struct_name {
                    #struct_name::new(#struct_name::#name)
                }
            }
        } else {
            quote! {
                pub fn #with_fn<T: Into<#repr>>(val: T) -> #struct_name {
                    let mut r = #struct_name::new(0 as #repr);
                    r.#set_call(val);
                    r
                }
            }
        }
    }

    /// Generate the Display printer for this field.
    pub fn gen_display(&self) -> quote::__rt::TokenStream {
        let name = &self.name;
        let lower = &self.lower_name;
        if self.region.len() == 1 {
            quote! {
                if self.#lower() {
                    if is_first { is_first = false; } else { write!(f, " | ")?; }
                    write!(f, stringify!(#name))?;
                }
            }
        } else {
            quote! {
                if is_first { is_first = false; } else { write!(f, " | ")?; }
                write!(f, "{}={:#X}", stringify!(#name), self.#lower())?;
            }
        }
    }

    /// Generates methods to operate on single-bit regions.
    /// Setter methods do not take values and includes a toggle method.
    fn gen_single_bit_ops(&self, struct_name: &syn::Ident)
        -> quote::__rt::TokenStream
    {
        let mask = format_ident!("{}", self.name);
        let lower = &self.lower_name;

        let set = format_ident!("set_{}", lower);
        let unset = format_ident!("unset_{}", lower);
        let toggle = format_ident!("toggle_{}", lower);
        let extract = format_ident!("extract_{}", lower);

        let getters = quote! {
            pub fn #lower(&self) -> bool {
                (self.0 & #struct_name::#mask) != 0
            }
            pub fn #extract(&self) -> #struct_name {
                #struct_name(self.0 & #struct_name::#mask)
            }
        };

        let setters = quote! {
            pub fn #set(&mut self) {
                self.0 |= #struct_name::#mask
            }
            pub fn #unset(&mut self) {
                self.0 &= !#struct_name::#mask
            }
            pub fn #toggle(&mut self) {
                self.0 ^= #struct_name::#mask
            }
        };

        quote!{
            #getters
            #setters
        }
    }

    /// Generates methods to operate on multi-bit regions.
    /// Setter methods take values and include debug_assert! calls for both
    /// bit-region as well as the optional value-range.
    fn gen_region_ops(&self, struct_name: &syn::Ident, repr: &syn::Type)
        -> quote::__rt::TokenStream
    {
        let mask = format_ident!("{}", self.name);
        let lower = &self.lower_name;

        let set = format_ident!("set_{}", lower);
        let extract = format_ident!("extract_{}", lower);

        let lower_tuple = format_ident!("{}_tuple", lower);
        let lower_bools = format_ident!("{}_bools", lower);
        let region_len = self.region.len();

        let bools_repr = {
            let as_str = (0..region_len)
                .map(|_| quote!{bool}).collect::<Vec<quote::__rt::TokenStream>>();
            quote! { (#(#as_str, )*) }
        };
        let bools_result = (0..region_len).enumerate().rev()
            .map(|(i, _)| quote!{(val >> #i) & 1 == 1}).collect::<Vec<quote::__rt::TokenStream>>();

        let tuple_repr = {
            let as_str = (0..region_len)
                .map(|_| quote!{u8}).collect::<Vec<quote::__rt::TokenStream>>();
            quote! { (#(#as_str, )*) }
        };
        let tuple_result = (0..region_len).enumerate().rev()
            .map(|(i, _)| quote!{if (val >> #i) & 1 == 1 { 1u8 } else { 0u8 }}).collect::<Vec<quote::__rt::TokenStream>>();

        let shift_offset = self.region.shift_offset();
        let value_assert = format!(
            "attempted to set {}::{} with value outside of region: {{:#X}}",
            struct_name, self.name);

        let range_assert = format!(
            "attempted to set {}::{} with value outside of range ({{:?}}): {{:#X}}",
            struct_name, self.name);
        let range_check = self.region.range.as_ref().map(|ref e| quote! {
                debug_assert!((#e).contains(&typed), #range_assert, (#e), typed);
        });

        let value_repr = match self.region.len() {
            0..=8   => { quote! { u8 } }
            9..=16  => { quote! { u16 } }
            17..=32 => { quote! { u32 } }
            33..=64 => { quote! { u64 } }
            _       => { quote! { usize } }
        };

        let (upshift, downshift) = if self.region.shift_offset() > 0 {
            (Some(quote!{ << #shift_offset }),
             Some(quote!{ >> #shift_offset }))
        } else {
            (None, None)
        };

        let getters = quote! {
            pub fn #lower(&self) -> #value_repr {
                ((self.0 & #struct_name::#mask) #downshift) as #value_repr
            }
            pub fn #lower_tuple(&self) -> #tuple_repr {
                let val = self.#lower();
                (#(#tuple_result, )*)
            }
            pub fn #lower_bools(&self) -> #bools_repr {
                let val = self.#lower();
                (#(#bools_result, )*)
            }
            pub fn #extract(&self) -> #struct_name {
                #struct_name(self.0 & #struct_name::#mask)
            }
        };

        let setters = quote! {
            pub fn #set<T: Into<#repr>>(&mut self, raw: T) {
                let typed: #repr = raw.into();
                let val: #repr = (typed #upshift) as #repr;
                #range_check
                debug_assert!(val & #struct_name::#mask == val, #value_assert, val);

                let mut tmp: #repr = self.0 & (!#struct_name::#mask);
                // TODO: may not be able to write entire word (allow slicing?)
                self.0 = tmp | val;
            }
        };

        quote!{
            #getters
            #setters
        }
    }
}

impl syn::parse::Parse for Field {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let name: syn::Ident = input.parse()?;
        let lower_str = format!("{}", name).trim().to_lowercase();
        let lower_name = syn::Ident::new(&lower_str, name.span());
        let _: syn::Token![:] = input.parse()?;
        let region: Region = input.parse()?;

        // check for gaps
        if region.has_gaps() {
            return Err(syn::Error::new(
                region.lit.span(), "region cannot contain gap(s)"));
        }

        Ok(Field { name, lower_name, region })
    }
}


/// Region contains metadata about a bit-region including the literal
/// expression, the mask, and a range if defined.
#[derive(Clone)]
struct Region {
    lit: syn::LitInt,
    value: usize,
    range: Option<syn::ExprRange>,
}
impl Region {
    /// Minimum number of bits needed to represent the mask literal
    pub fn min_value_bits(&self) -> usize {
        (std::mem::size_of::<usize>()*8) - (self.value.leading_zeros() as usize) - 1
    }

    /// Number of bits in the region
    pub fn len(&self) -> usize {
        self.value.count_ones() as usize
    }

    /// Offset required to shift "1" to the least significant bit in the region
    pub fn shift_offset(&self) -> usize {
        self.value.trailing_zeros() as usize
    }

    /// Check if the defined mask contains gaps
    pub fn has_gaps(&self) -> bool {
        (self.len() + self.shift_offset() - 1) != self.min_value_bits()
    }

    /// Check if this region intersects with another
    pub fn intersects(&self, other: &Self) -> bool {
        let self_min = self.shift_offset();
        let self_max = self_min + self.len() - 1;

        let oth_min = other.shift_offset();
        let oth_max = oth_min + other.len() - 1;

        if self_max <= oth_max && self_max >= oth_min {
            true
        } else if self_min >= oth_min && self_min <= oth_max {
            true
        } else {
            false
        }
    }
}

impl syn::parse::Parse for Region {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let lit: syn::LitInt = input.parse()?;
        let value = lit.base10_digits().parse().expect("failed to parse literal");

        let mut range = None;
        if input.peek(syn::Token![|]) {
            let _: syn::Token![|] = input.parse()?;
            range = Some(input.parse()?);
        }

        Ok(Region {
            lit,
            value,
            range,
        })
    }
}


struct UserFns {
    fns: Vec<syn::ItemFn>,
}

impl syn::parse::Parse for UserFns {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let mut fns = vec!();

        while input.peek(syn::token::Pub) || input.peek(syn::token::Fn) {
            fns.push(input.parse()?);
        }

        Ok(UserFns {
            fns,
        })
    }
}


#[proc_macro]
pub fn bitregions(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(item as BitRegions);
    let vis = &input.vis;
    let name = &input.struct_def.ident;
    let repr = &input.struct_def.repr;
    let user_fns = &input.user_fns.fns;

    // create token streams for the const-defs of the masks
    let mask_defs = input.struct_def.fields.iter().map(|f| {
        let val = &f.region.lit;
        let mask = &f.name;
        quote! { pub const #mask: #repr = #val; }
    }).collect::<Vec<quote::__rt::TokenStream>>();

    // generate token stream for (optional) default
    let default = input.struct_def.default_loc.map(|m| {
        let expr = match m {
            MemoryLocation::Ident(i) => { quote! { #i } },
            MemoryLocation::Lit(l) => { quote! { #l } },
            MemoryLocation::Expr(e) => { quote! { #e } },
        };
        quote!{
            pub unsafe fn default_ptr() -> &'static mut Self {
                Self::at_addr_mut(#expr)
            }
        }
    });

    // generate token streams for the "with_{field}" constructors
    let with_ctors = input.struct_def.fields.iter().map(|f| f.gen_with_ctor(name, repr))
        .collect::<Vec<quote::__rt::TokenStream>>();

    // generate token streams for the methods
    let mask_ops = input.struct_def.fields.iter().map(|f| f.gen_ops(name, repr))
        .collect::<Vec<quote::__rt::TokenStream>>();

    // generate token streams for the field Display impls
    let display_ops = input.struct_def.fields.iter().map(|f| f.gen_display())
        .collect::<Vec<quote::__rt::TokenStream>>();

    // make display and debug impls
    let display_debug = quote! {
        impl core::fmt::Display for #name {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                let mut is_first = true;
                #( #display_ops )*
                Ok(())
            }
        }

        impl core::fmt::Debug for #name {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                write!(f, "{:#X}", self.0)
            }
        }
    };

    let result = quote! {
        #[repr(C)]
        #[derive(Copy, Clone)]
        #vis struct #name(#repr);

        #display_debug

        impl #name {
            #(#mask_defs)*

            pub fn raw(&self) -> #repr {
                self.0
            }

            pub fn new<T: Into<#repr>>(bits: T) -> #name {
                #name(bits.into())
            }

            pub unsafe fn at_addr<'a>(addr: usize) -> &'a #name {
                &*(addr as *const u8 as *const #name)
            }

            pub unsafe fn at_addr_mut<'a>(addr: usize) -> &'a mut #name {
                &mut *(addr as *mut u8 as *mut #name)
            }

            pub unsafe fn at_ref<'a, T>(r: &T) -> &'a #name {
                &*(r as *const T as *const #name)
            }

            pub unsafe fn at_ref_mut<'a, T>(r: &mut T) -> &'a mut #name {
                &mut *(r as *mut T as *mut #name)
            }

            #default
            #(#with_ctors)*


            #(#user_fns)*

            #(#mask_ops)*
        }
        impl Into<#repr> for #name {
            fn into(self) -> #repr {
                self.0
            }
        }
        impl From<#repr> for #name {
            fn from(val: #repr) -> Self {
                Self::new(val)
            }
        }

        impl PartialEq for #name {
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0
            }
        }

        //
        // add
        //
        impl<T: Into<#repr>> core::ops::Add<T> for #name {
            type Output = Self;
            fn add(self, other: T) -> Self::Output {
                #name(self.0 + other.into())
            }
        }
        impl<T: Into<#repr>> core::ops::AddAssign<T> for #name {
            fn add_assign(&mut self, other: T) {
                self.0 += other.into();
            }
        }

        //
        // sub
        //
        impl<T: Into<#repr>> core::ops::Sub<T> for #name {
            type Output = Self;
            fn sub(self, other: T) -> Self::Output {
                #name(self.0 - other.into())
            }
        }
        impl<T: Into<#repr>> core::ops::SubAssign<T> for #name {
            fn sub_assign(&mut self, other: T) {
                self.0 -= other.into();
            }
        }

        //
        // mul
        //
        impl<T: Into<#repr>> core::ops::Mul<T> for #name {
            type Output = Self;
            fn mul(self, other: T) -> Self::Output {
                #name(self.0 * other.into())
            }
        }
        impl<T: Into<#repr>> core::ops::MulAssign<T> for #name {
            fn mul_assign(&mut self, other: T) {
                self.0 *= other.into();
            }
        }

        //
        // div
        //
        impl<T: Into<#repr>> core::ops::Div<T> for #name {
            type Output = Self;
            fn div(self, other: T) -> Self::Output {
                #name(self.0 / other.into())
            }
        }
        impl<T: Into<#repr>> core::ops::DivAssign<T> for #name {
            fn div_assign(&mut self, other: T) {
                self.0 /= other.into();
            }
        }

        //
        // bitor
        //
        impl<T: Into<#repr>> core::ops::BitOr<T> for #name {
            type Output = Self;
            fn bitor(self, other: T) -> Self::Output {
                #name(self.0 | other.into())
            }
        }
        impl<T: Into<#repr>> core::ops::BitOrAssign<T> for #name {
            fn bitor_assign(&mut self, other: T) {
                self.0 |= other.into();
            }
        }

        //
        // bitand
        //
        impl<T: Into<#repr>> core::ops::BitAnd<T> for #name {
            type Output = Self;
            fn bitand(self, other: T) -> Self::Output {
                #name(self.0 & other.into())
            }
        }
        impl<T: Into<#repr>> core::ops::BitAndAssign<T> for #name {
            fn bitand_assign(&mut self, other: T) {
                self.0 &= other.into();
            }
        }


        //
        // bitxor
        //
        impl<T: Into<#repr>> core::ops::BitXor<T> for #name {
            type Output = Self;
            fn bitxor(self, other: T) -> Self::Output {
                #name(self.0 ^ other.into())
            }
        }
        impl<T: Into<#repr>> core::ops::BitXorAssign<T> for #name {
            fn bitxor_assign(&mut self, other: T) {
                self.0 ^= other.into();
            }
        }

        //
        // shr
        //
        impl<T: Into<#repr>> core::ops::Shr<T> for #name {
            type Output = Self;
            fn shr(self, other: T) -> Self::Output {
                #name(self.0 >> other.into())
            }
        }
        impl<T: Into<#repr>> core::ops::ShrAssign<T> for #name {
            fn shr_assign(&mut self, other: T) {
                self.0 >>= other.into();
            }
        }

        //
        // shl
        //
        impl<T: Into<#repr>> core::ops::Shl<T> for #name {
            type Output = Self;
            fn shl(self, other: T) -> Self::Output {
                #name(self.0 << other.into())
            }
        }
        impl<T: Into<#repr>> core::ops::ShlAssign<T> for #name {
            fn shl_assign(&mut self, other: T) {
                self.0 <<= other.into();
            }
        }
    };

    result.into()
}
