use sr_primitives::traits::{Zero};
use codec::{Decode, Encode};
use crate::Trait;

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq, Eq)]
pub enum KittySex {
	Female, Male
}

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq, Eq)]
pub enum KittyElement {
	Natural,
	Metal,
	Wood,
	Water,
	Fire,
	Earth
}

#[cfg_attr(feature = "std", derive(Debug, PartialEq, Eq))]
#[derive(Encode, Decode)]
pub struct Kitty<T: Trait> {
	birth: T::Moment,
	generation: u8,
	appearance: [u8; 6],
	sex: KittySex,
	health: u8,
	attack: u8,
	defence: u8,
	stamina: u8,
	element: KittyElement,
}

impl<T: Trait> Kitty<T> {
	pub fn new() -> Kitty<T> {
		Kitty {
			birth: Zero::zero(),
			generation: 0,
			appearance: [0; 6],
			sex: KittySex::Female,
			health: 0,
			attack: 0,
			defence: 0,
			stamina: 0,
			element: KittyElement::Natural,
		}
	}
}
