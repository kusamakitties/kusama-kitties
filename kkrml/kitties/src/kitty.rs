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

#[cfg_attr(feature = "std", derive(Debug))]
#[derive(Encode, Decode, PartialEq, Eq, Clone, Copy)]
pub enum Rarity {
	Common,
	Rare,
	Epic,
}

impl Rarity {
	fn chance(&self) -> u8 {
		match self {
			Rarity::Common => 4,
			Rarity::Rare => 2,
			Rarity::Epic => 1
		}
	}

	fn base_value(&self) -> u8 {
		match self {
			Rarity::Common => 0,
			Rarity::Rare => 100,
			Rarity::Epic => 170,
		}
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

// number of items in different rarity (common, rare, epic)
// be careful to make sure the counts never exceeds Rarity::base_value and overflow u8
const APPEARANCE_ITEM_COUNT: [(u8, u8, u8); 6] = [
	// accessories
	(16, 4, 0),
	// body
	(12, 3, 0),
	// eyes
	(12, 15, 0),
	// mouth
	(8, 2, 0),
	// fur
	(8, 2, 0),
	// zz
	(1, 1, 1),
];

fn generate_appearance(counts: &(u8, u8, u8), rng: &mut impl Rng) -> u8 {
	use Rarity::*;
	// be careful this should not overflow u8 otherise will need to upsize the type
	let common_max = counts.0 * Common.chance();
	let rare_max = counts.1 * Rare.chance();
	let epic_max = counts.2 * Epic.chance();
	let max = common_max + rare_max + epic_max;
	let value = rng.gen_range(0, max);
	if value < common_max {
		Common.base_value() + value / Common.chance()
	} else if value < rare_max {
		Rare.base_value() + value / Rare.chance()
	} else {
		Epic.base_value() + value / Epic.chance()
	}

}

impl<T: Trait> Kitty<T> {
	pub fn new(rng: &mut impl Rng) -> Kitty<T> {
		let stats_range = Uniform::new(0, 5);
		let mut appearance = [0u8; 6];
		for i in 0..6 {
			appearance[i] = generate_appearance(&APPEARANCE_ITEM_COUNT[i], rng);
		}
		Kitty {
			birth: <timestamp::Module<T>>::now(),
			generation: 0,
			appearance,
			sex: rng.gen(),
			health: stats_range.sample(rng),
			attack: stats_range.sample(rng),
			defence: stats_range.sample(rng),
			stamina: stats_range.sample(rng),
			element: rng.gen(),
		}
	}
}
