//
// Copyright (c) Zach Marcantel. All rights reserved.
// Licensed under the GPLv3. See LICENSE file in the project root
// for full license information.
//

pub use bitregions_impl::bitregions;

#[cfg(test)]
mod test {
    use super::*;

    bitregions! {
        pub Test u16 {
            LOW_REGION:     0b00000111,
            HIGH_REGION:    0b00011000,
            HIGH_TOGGLE:    0b01000000,
        }
    }

    // TODO: test overlap funcs
    // TODO: should_panic tests around generation?
    /*
    bitregions! {
        pub TestOverlap u16 {
            LOW_REGION:     0b00000111,
            HIGH_REGION:    0b00011100,
        }
    }
    */


    // TODO: test gaps funcs
    // TODO: should_panic tests around generation?
    /*
    bitregions! {
        pub TestGap u16 {
            GAP:    0b00101000,
        }
    }
    */

    #[test]
    fn get_set_low_region() {
        let mut test = Test::from(0);

        assert!(0 == test.low_region());
        test.set_low_region(5u8);
        assert!(5 == test.low_region());
        test.set_low_region(2u8);
        assert!(2 == test.low_region());
        test.set_low_region(7u8);
        assert!(7 == test.low_region());
        test.set_low_region(0u8);
        assert!(0 == test.low_region());
    }

    #[test]
    #[should_panic]
    fn set_beyond_low_region() {
        let mut test = Test::from(0);
        test.set_low_region(8u8);
    }

    #[test]
    fn get_set_high_region() {
        let mut test = Test::from(0);

        assert!(0 == test.high_region());
        test.set_high_region(2u8);
        assert!(2 == test.high_region());
        test.set_low_region(3u8);
        assert!(3 == test.low_region());
        test.set_high_region(0u8);
        assert!(0 == test.high_region());
    }

    #[test]
    #[should_panic]
    fn set_beyond_high_region() {
        let mut test = Test::from(0);
        test.set_high_region(4u8);
    }


    #[test]
    fn toggle() {
        let mut test = Test::from(0);

        assert!(!test.high_toggle());
        test.toggle_high_toggle();
        assert!(test.high_toggle());
        test.toggle_high_toggle();
        assert!(!test.high_toggle());
    }

    #[test]
    fn set_single_bit() {
        let mut test = Test::from(0);

        assert!(!test.high_toggle());
        test.set_high_toggle();
        assert!(test.high_toggle());
        test.set_high_toggle();
        assert!(test.high_toggle());
    }

    #[test]
    fn unset_single_bit() {
        let mut test = Test::from(0);

        assert!(!test.high_toggle());
        test.set_high_toggle();
        assert!(test.high_toggle());
        test.unset_high_toggle();
        assert!(!test.high_toggle());
        test.unset_high_toggle();
        assert!(!test.high_toggle());
    }

    #[test]
    fn at_addr() {
        let mut mem: [u8; 4096] = [0u8; 4096];
        let u16_ptr = &mut mem[8] as *mut _ as *mut u16;
        let test = unsafe {
            Test::at_addr_mut(&mut mem[8] as *mut _ as usize)
        };

        assert_eq!(unsafe{*u16_ptr}, 0);
        assert_eq!(unsafe{*u16_ptr}, test.raw());

        test.set_high_region(3u8);
        assert_eq!(unsafe{*u16_ptr}, 0b11000);
        assert_eq!(unsafe{*u16_ptr}, test.raw());

        test.set_low_region(5u8);
        assert_eq!(unsafe{*u16_ptr}, 0b11101);
        assert_eq!(unsafe{*u16_ptr}, test.raw());

        test.toggle_high_toggle();
        assert_eq!(unsafe{*u16_ptr}, 0b1011101);
        assert_eq!(unsafe{*u16_ptr}, test.raw());

        test.toggle_high_toggle();
        assert_eq!(unsafe{*u16_ptr}, 0b0011101);
        assert_eq!(unsafe{*u16_ptr}, test.raw());
    }

    #[test]
    fn at_ref() {
        let mut mem: [u8; 4096] = [0u8; 4096];
        let u16_ptr = &mut mem[16] as *mut _ as *mut u16;
        let test = unsafe { Test::at_ref_mut(&mut mem[16]) };

        assert_eq!(unsafe{*u16_ptr}, 0);
        assert_eq!(unsafe{*u16_ptr}, test.raw());

        test.set_high_region(3u8);
        assert_eq!(unsafe{*u16_ptr}, 0b11000);
        assert_eq!(unsafe{*u16_ptr}, test.raw());

        test.set_low_region(5u8);
        assert_eq!(unsafe{*u16_ptr}, 0b11101);
        assert_eq!(unsafe{*u16_ptr}, test.raw());

        test.toggle_high_toggle();
        assert_eq!(unsafe{*u16_ptr}, 0b1011101);
        assert_eq!(unsafe{*u16_ptr}, test.raw());

        test.toggle_high_toggle();
        assert_eq!(unsafe{*u16_ptr}, 0b0011101);
        assert_eq!(unsafe{*u16_ptr}, test.raw());
    }

    #[test]
    fn math_ops() {
        // add
        assert_eq!(Test::new(18u16), Test::new(3u16) + Test::new(15u16));
        // sub
        assert_eq!(Test::new(7u16), Test::new(20u16) - Test::new(13u16));
        // mul
        assert_eq!(Test::new(18u16), Test::new(6u16) * Test::new(3u16));
        // div
        assert_eq!(Test::new(3u16), Test::new(24u16) / Test::new(8u16));
    }

    #[test]
    fn math_assign_ops() {
        // add
        let mut add_test = Test::new(3u16);
        add_test += Test::new(15u16);
        assert_eq!(Test::new(18u16), add_test);
        // sub
        let mut sub_test = Test::new(20u16);
        sub_test -= Test::new(13u16);
        assert_eq!(Test::new(7u16), sub_test);
        // mul
        let mut mul_test = Test::new(6u16);
        mul_test *= Test::new(3u16);
        assert_eq!(Test::new(18u16), mul_test);
        // div
        let mut div_test = Test::new(24u16);
        div_test /= Test::new(8u16);
        assert_eq!(Test::new(3u16), div_test);
    }


    #[test]
    fn bit_ops() {
        // or
        assert_eq!(Test::new(0xB4u16), Test::new(0xB0u16) | Test::new(0x04u16));
        // and
        assert_eq!(Test::new(0x04u16), Test::new(0xD7u16) & Test::new(0x04u16));
        // xor
        assert_eq!(Test::new(0b001100u16), Test::new(0b110011u16) ^ Test::new(0b111111u16));
        // shl
        assert_eq!(Test::new(8u16), Test::new(2u16) << Test::new(2u16));
        // shr
        assert_eq!(Test::new(1u16), Test::new(8u16) >> Test::new(3u16));
    }

    #[test]
    fn bit_assign_ops() {
        // or
        let mut or_test = Test::new(0xB0u16);
        or_test |= Test::new(0x04u16);
        assert_eq!(Test::new(0xB4u16), or_test);
        // and
        let mut and_test = Test::new(0xD7u16);
        and_test &= Test::new(0x04u16);
        assert_eq!(Test::new(4u16), and_test);
        // xor
        let mut xor_test = Test::new(0b110011u16);
        xor_test ^= Test::new(0b111111u16);
        assert_eq!(Test::new(0b001100u16), xor_test);
        // shl
        let mut shl_test = Test::new(1u16);
        shl_test <<= Test::new(3u16);
        assert_eq!(Test::new(8u16), shl_test);
        // shr
        let mut shr_test = Test::new(8u16);
        shr_test >>= Test::new(3u16);
        assert_eq!(Test::new(1u16), shr_test);
    }
}
