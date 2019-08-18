use codec::{Decode, Encode};
use rand::{
	Rng,
	seq::SliceRandom,
	distributions::{Distribution, Uniform, Standard}
};
use crate::Trait;

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq, Eq, Clone, Copy)]
pub enum KittySex {
	Female, Male
}

impl Distribution<KittySex> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> KittySex {
        use self::KittySex::*;
		*[Female, Male].choose(rng).unwrap()
    }
}

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq, Eq, Clone, Copy)]
pub enum KittyElement {
	Natural,
	Metal,
	Wood,
	Water,
	Fire,
	Earth
}

impl Distribution<KittyElement> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> KittyElement {
        use self::KittyElement::*;
        *[
			Natural,
			Metal,
			Wood,
			Water,
			Fire,
			Earth
		].choose(rng).unwrap()
    }
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
	pub fn new(rng: &mut impl Rng) -> Kitty<T> {
		let stats_range = Uniform::new(0, 5);
		Kitty {
			birth: <timestamp::Module<T>>::now(),
			generation: 0,
			appearance: rng.gen(),
			sex: rng.gen(),
			health: stats_range.sample(rng),
			attack: stats_range.sample(rng),
			defence: stats_range.sample(rng),
			stamina: stats_range.sample(rng),
			element: rng.gen(),
		}
	}
}
