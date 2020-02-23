bitregions
============

Generate a unit structure to represent a set of bit-regions.
Intended to be used both as bitflags held in structs/collections as well
as representing something like a memory-mapped register in more embedded
applications.

This crate is set as `#![no_std]` so it can freely be used in other such crates.

Regions are given the `#repr({type})` attribute based on the `{repr}`
given to the macro.

The following traits are generated for the new struct:
- `Into<{repr}>`
- `From<{repr}>`
- `PartialEq`
- `Display`
    - toggles print their name if set
    - multibit always prints `{name}={val}`
- `Debug`
    - prints raw value in hex
- `+` and `+=`
- `-` and `-=`
- `*` and `*=`
- `/` and `/=`
- `^` and `^=`
- `|` and `|=`
- `&` and `&=`


Examples
=================


Basic Example:
-----------------

Example purely to show the API.
Creates a stack-based u16 unit-struct with helper methods.

```
# #[macro_use] extern crate bitregions;
bitregions! {
    pub Example u16 {
        EN_FEATURE:   0b0000000000000001,
        EN_DEVICE:    0b0000000000000010,
        PORT_NUM:     0b0000000000011100 | 0..=5, // only 0-5 is valid
        BUSY:         0b0000000001000000,
        VAL_BUFFER:   0b1111111100000000,
    }
}

fn main() {
    println!("value buffer mask is: {:#X}", Example::VAL_BUFFER);

    // create an example memory mapped io register
    // exists on the stack with the value 0.
    // see below for using this as a pointer to the register.
    let mut ex = Example::new(0u16);

    // enable the feature this register governs
    ex.set_en_feature();

    // wait for the busy bit to clear
    // then set busy to block reader (could be more pedantic with ex.set_busy())
    while ex.busy() { println!("bus is busy"); }
    ex.toggle_busy();
    assert_eq!(ex.extract_busy().raw() & Example::BUSY, Example::BUSY);

    // set the port to write to. must be 0-5
    // otherwise we trigger a debug_assert! (removed in release builds)
    ex.set_port_num(4u8);
    // put the value into the buffer
    ex.set_val_buffer(0x38u8);
    // clear busy bit (could be more pedantic with ex.unset_busy())
    ex.toggle_busy();

    // wait for a response
    while ex.busy() { println!("waiting for response"); }

    // read the value out of the buffer (pre-shifted for you)
    // then, assert the shift happened correctly by looking at the
    // unshifted version returned by the extract_{field} variant.
    let resp = ex.val_buffer();
    assert_eq!(resp << 8, ex.extract_val_buffer().raw());

    // disable the feature this register governs
    ex.unset_en_feature();


    //
    // math and bitwise operations
    //

    ex += 1u16.into();
    ex -= 1u16.into();
    ex *= 2u16.into();
    ex /= 2u16.into();
    ex |= 0xBD.into();
    ex &= 0xDB.into();
    ex ^= ex.raw().into();

    //
    // display and debug
    //

    ex = Example::new(0u16);
    ex.set_en_feature();
    ex.set_port_num(4u8);
    ex.set_val_buffer(0xABu8);
    let display = format!("{}", ex);
    assert_eq!(display, "EN_FEATURE | PORT_NUM=0x4 | VAL_BUFFER=0xAB");

    let debug = format!("{:?}", ex);
    assert_eq!(debug, "0xAB11");
}
```


Memory-mapped Example:
------------------------

A common case for bitmaps/bitflags/etc are memory-mapped registers.
Below is an example that creates a lifetimed reference to some memory
region this register would represent.

You can optionally provide a default address location using the
`{name} {repr} @ {addr}` syntax. This variant returns a static, mutable ref.

```
# #[macro_use] extern crate bitregions;
bitregions! {
    pub Example u16 @ 0xDEADBEEF {
        EN_FEATURE:   0b0000000000000001,
        EN_DEVICE:    0b0000000000000010,
        PORT_NUM:     0b0000000000011100 | 0..=5, // only 0-5 is valid
        BUSY:         0b0000000001000000,
        VAL_BUFFER:   0b1111111100000000,
    }
}

const MEMIO_ADDR: usize = 0xC0FFEE;
bitregions! {
    pub MemIOBase u16 @ MEMIO_ADDR {
        SOME_REGION:  0b0000000000000001,
    }
}
bitregions! {
    pub ControlReg u16 @ MEMIO_ADDR + 0x80 {
        SOME_REGION:  0b0000000000000001,
    }
}


fn main() {
    // create "fake memory" so the doc-test works
    // address is the important thing
    let mem: [u8; 4096] = [0u8; 4096];

    // create a lifetimed reference to the register elsewhere
    // in memory (the above slice, in our case, but could be anywhere)
    let ex = unsafe { Example::at_addr_mut(&mem[8] as *const _ as usize) };

    // everything else works like normal
    ex.set_en_feature();
    assert!(ex.en_feature());
    ex.set_val_buffer(128u8);
    println!("{:#X}", ex.val_buffer());
    assert_eq!(128, ex.val_buffer());

    // you can also initialize the pointer directly
    let ptr = unsafe { Example::default_ptr() };
    assert_eq!(ptr as *mut _ as usize, 0xDEADBEEF);
    // but we cannot use it in the examples or it will segfault :/

    // you can set the default address using a literal, ident, or const expression
    let memio = unsafe { MemIOBase::default_ptr() };
    assert_eq!(memio as *mut _ as usize, MEMIO_ADDR);
    let control = unsafe { ControlReg::default_ptr() };
    assert_eq!(control as *mut _ as usize, MEMIO_ADDR + 0x80);
}
```



From Reference Example:
---------------------------

Below is an example which casts a reference of the region's underlying
type to our generated struct. This allows you to "add features" to a raw
value. While safer than the memory-mapped example but is still unsafe code
as you could share a reference into a slice.

```
# #[macro_use] extern crate bitregions;
bitregions! {
    pub Example u16 {
        EN_FEATURE:   0b0000000000000001,
        EN_DEVICE:    0b0000000000000010,
        PORT_NUM:     0b0000000000011100 | 0..=5, // only 0-5 is valid
        BUSY:         0b0000000001000000,
        VAL_BUFFER:   0b1111111100000000,
    }
}


fn main() {
    // create "fake memory" to illustrate the example
    // the reference could be to a single u16 or relevant type...
    let mut mem: [u8; 4096] = [0u8; 4096];

    // create the reference -- this is unsafe because we allow
    // for a wider range of types than strictly the underlying type.
    // you can see in this example we use a &u8 to create (effectively) a &u16
    let ex = unsafe { Example::at_ref_mut(&mut mem[8]) };

    // everything else works like normal
    ex.set_en_feature();
    assert!(ex.en_feature());
    ex.set_val_buffer(128u8);
    println!("{:#X}", ex.val_buffer());
    assert_eq!(128, ex.val_buffer());
}
```



Debug Assertions
----------------------

When built in debug-mode, setters will assert the given value
both fits in the region (4bit number in 2bit region) and is within
the (optional) range (3bit region, 0-5 allowed, given 7).

```should_panic
# #[macro_use] extern crate bitregions;
bitregions! {
    pub Example u8 {
        RANGED:     0b00011100 | 1..=6,
        NON_RANGED: 0b11100000,
    }
}


fn main() {
    let mut ex = Example::new(0u8);

    ex.set_ranged(1u8); // works fine
    ex.set_ranged(3u8); // works fine
    ex.set_ranged(6u8); // works fine
    ex.set_ranged(0u8); // will panic do to range violation
    ex.set_ranged(7u8); // will panic do to range violation
    ex.set_ranged(8u8); // will panic do to region violation

    ex.set_non_ranged(1u8); // works fine
    ex.set_non_ranged(3u8); // works fine
    ex.set_non_ranged(6u8); // works fine
    ex.set_non_ranged(0u8); // works fine
    ex.set_non_ranged(7u8); // works fine
    ex.set_non_ranged(8u8); // will panic do to region violation
}
```


