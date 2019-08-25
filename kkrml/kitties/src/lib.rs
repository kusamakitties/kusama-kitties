#![cfg_attr(not(feature = "std"), no_std)]

use rand::SeedableRng;
use rstd::prelude::*;
use rstd::result;
use rstd::{cmp, convert::{TryInto, Into}};
use support::{decl_event, decl_module, decl_storage, ensure, traits::Currency, traits::Get, StorageMap, StorageValue};
use system::ensure_signed;

mod linked_item;
use linked_item::{LinkedItem, LinkedList};

mod kitty;
use kitty::{Kitty, KittyDetails, KittyIndex};

mod random;
use random::{random_seed, Rng};

pub trait Trait: timestamp::Trait {
	type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
	type Currency: Currency<Self::AccountId>;

	type ClaimSecondsPerExp: Get<Self::Moment>;
	type ClaimCurrencyPerSecond: Get<BalanceOf<Self>>;
	type ClaimSecondsMax: Get<Self::Moment>;
}

type BalanceOf<T> = <<T as Trait>::Currency as Currency<<T as system::Trait>::AccountId>>::Balance;

type KittyLinkedItem = LinkedItem<KittyIndex>;
type OwnedKittiesList<T> = LinkedList<OwnedKitties<T>, <T as system::Trait>::AccountId, KittyIndex>;

fn rng<T: Trait>(sender: &T::AccountId) -> Rng {
	Rng::from_seed(random_seed::<T, _>((b"kty", sender)))
}

decl_storage! {
	trait Store for Module<T: Trait> as Kitties {
		/// Stores all the kitties, key is the kitty id / index
		pub Kitties get(kitty): map KittyIndex => Option<Kitty<T>>;

		/// The next kitty index
		pub NextKittyIndex get(next_kitty_index): Option<KittyIndex> = Some(Default::default());

		/// Get kitty name by kitty id
		pub KittiesName get(kitty_name): map KittyIndex => Vec<u8>;

		/// Get kitty exp by kitty id
		pub KittiesExp get(kitty_exp): map KittyIndex => u32;

		/// Get kitty awake time by kitty id
		pub KittiesAwakeTime get(kitty_awake_time): map KittyIndex => T::Moment;

		/// Get kitty last claim by kitty id
		pub KittiesLastClaimTime get(kitty_last_claim_time): map KittyIndex => T::Moment;

		/// Get kitty ownership. Stored in a linked map.
		pub OwnedKitties get(owned_kitties): map (T::AccountId, Option<KittyIndex>) => Option<KittyLinkedItem>;

		/// Get kitty owner
		pub KittyOwners get(kitty_owner): map KittyIndex => Option<T::AccountId>;
	}
}

decl_event!(
	pub enum Event<T> where
		<T as system::Trait>::AccountId,
	{
		/// A new kitty is captured. (owner, kitty_id)
		Captured(AccountId, KittyIndex),
		/// A new kitty is born. (owner, kitty_id)
		Born(AccountId, KittyIndex),
		/// A kitty is transferred. (from, to, kitty_id)
		Transferred(AccountId, AccountId, KittyIndex),
	}
);

decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {

		const ClaimSecondsPerExp: T::Moment = T::ClaimSecondsPerExp::get();
		const ClaimCurrencyPerSecond: BalanceOf<T> = T::ClaimCurrencyPerSecond::get();
		const ClaimSecondsMax: T::Moment = T::ClaimSecondsMax::get();

		fn deposit_event<T>() = default;

		/// Create a new kitty
		pub fn create(origin) {
			let sender = ensure_signed(origin)?;

			// Create and store kitty
			let kitty = Kitty::new(&mut rng::<T>(&sender));
			let kitty_id = Self::insert_kitty(&sender, kitty)?;

			Self::deposit_event(RawEvent::Captured(sender, kitty_id));
		}

		pub fn update_name(origin, kitty_id: KittyIndex, name: Vec<u8>) {
			let sender = ensure_signed(origin)?;
			ensure!(<OwnedKitties<T>>::exists(&(sender.clone(), Some(kitty_id))), "Only owner can update kitty name");
			ensure!(name.len() < 50, "Kitty name cannot be more than 50 bytes");
			KittiesName::insert(kitty_id, name);
		}

		/// Breed kitties
		pub fn breed(origin, kitty_id_1: KittyIndex, kitty_id_2: KittyIndex) {
			let sender = ensure_signed(origin)?;

			let new_kitty_id = Self::do_breed(&sender, kitty_id_1, kitty_id_2)?;

			Self::deposit_event(RawEvent::Born(sender, new_kitty_id));
		}

		/// Transfer a kitty to new owner
		pub fn transfer(origin, to: T::AccountId, kitty_id: KittyIndex) {
			let sender = ensure_signed(origin)?;

			ensure!(<OwnedKitties<T>>::exists(&(sender.clone(), Some(kitty_id))), "Only owner can transfer kitty");

			Self::do_transfer(&sender, &to, kitty_id);

			Self::deposit_event(RawEvent::Transferred(sender, to, kitty_id));
		}

		pub fn claim(origin, kitty_id: KittyIndex) {
			let sender = ensure_signed(origin)?;
			ensure!(<OwnedKitties<T>>::exists(&(sender.clone(), Some(kitty_id))), "Only owner can claim their kitty rewards");

			// TODO: remove bunch clones when https://github.com/paritytech/substrate/pull/3476 is merged

			let last_claim = Self::kitty_last_claim_time(kitty_id);
			let now = timestamp::Module::<T>::now();
			let time_diff = cmp::min(now.clone() - last_claim, T::ClaimSecondsMax::get());

			let reward_exp = time_diff.clone() / T::ClaimSecondsPerExp::get();
			let reward_exp: u32 = TryInto::<u32>::try_into(reward_exp).map_err(|_| "Reward exp overflow")?;
			let new_exp = Self::kitty_exp(kitty_id).checked_add(reward_exp).ok_or("Kitty exp overflow")?;

			let time_diff = TryInto::<u32>::try_into(time_diff).map_err(|_| "Time diff overflow")?;
			let reward_currency = Into::<BalanceOf<T>>::into(time_diff) * T::ClaimCurrencyPerSecond::get();
			let reward_currency: BalanceOf<T> = reward_currency.into();

			T::Currency::deposit_into_existing(&sender, reward_currency)?;
			KittiesExp::insert(kitty_id, new_exp);
			KittiesLastClaimTime::<T>::insert(kitty_id, now);
		}
	}
}

impl<T: Trait> Module<T> {
	fn insert_owned_kitty(owner: &T::AccountId, kitty_id: KittyIndex) {
		<OwnedKittiesList<T>>::append(owner, kitty_id);
	}

	fn insert_kitty(owner: &T::AccountId, kitty: Kitty<T>) -> result::Result<KittyIndex, &'static str> {
		let kitty_id = Self::next_kitty_index().ok_or("Kitties count overflow")?;
		// Create and store kitty
		<Kitties<T>>::insert(kitty_id, kitty);
		if let Some(next_id) = kitty_id.next_index() {
			NextKittyIndex::put(next_id);
		} else {
			NextKittyIndex::kill();
		}

		<KittyOwners<T>>::insert(kitty_id, owner.clone());
		<KittiesLastClaimTime<T>>::insert(kitty_id, timestamp::Module::<T>::now());

		Self::insert_owned_kitty(owner, kitty_id);

		Ok(kitty_id)
	}

	fn do_breed(
		sender: &T::AccountId,
		kitty_id_1: KittyIndex,
		kitty_id_2: KittyIndex,
	) -> result::Result<KittyIndex, &'static str> {
		ensure!(kitty_id_1 != kitty_id_2, "Needs different parent");
		ensure!(
			Self::kitty_owner(&kitty_id_1)
				.map(|owner| owner == *sender)
				.unwrap_or(false),
			"Not onwer of kitty1"
		);
		ensure!(
			Self::kitty_owner(&kitty_id_2)
				.map(|owner| owner == *sender)
				.unwrap_or(false),
			"Not owner of kitty2"
		);

		let mut parent1 = KittyDetails::from(kitty_id_1).ok_or("Invalid kitty_id_1")?;
		let mut parent2 = KittyDetails::from(kitty_id_2).ok_or("Invalid kitty_id_2")?;
		let new_kitty = Kitty::from_parents(&mut parent1, &mut parent2, &mut rng::<T>(&sender))?;

		let kitty_id = Self::insert_kitty(sender, new_kitty)?;

		Ok(kitty_id)
	}

	fn do_transfer(from: &T::AccountId, to: &T::AccountId, kitty_id: KittyIndex) {
		<OwnedKittiesList<T>>::remove(&from, kitty_id);
		<OwnedKittiesList<T>>::append(&to, kitty_id);
		<KittyOwners<T>>::insert(kitty_id, to);
	}
}

/// Tests for Kitties module
#[cfg(test)]
mod tests {
	use super::*;

	use primitives::{Blake2Hasher, H256};
	use runtime_io::with_externalities;
	use sr_primitives::{
		testing::Header,
		traits::{BlakeTwo256, IdentityLookup},
		Perbill,
	};
	use support::{impl_outer_origin, parameter_types};

	impl_outer_origin! {
		pub enum Origin for Test {}
	}

	// For testing the module, we construct most of a mock runtime. This means
	// first constructing a configuration type (`Test`) which `impl`s each of the
	// configuration traits of modules we want to use.
	#[derive(Clone, Eq, PartialEq, Debug)]
	pub struct Test;
	parameter_types! {
		pub const BlockHashCount: u64 = 250;
		pub const MaximumBlockWeight: u32 = 1024;
		pub const MaximumBlockLength: u32 = 2 * 1024;
		pub const AvailableBlockRatio: Perbill = Perbill::one();
	}
	impl system::Trait for Test {
		type Origin = Origin;
		type Index = u64;
		type BlockNumber = u64;
		type Call = ();
		type Hash = H256;
		type Hashing = BlakeTwo256;
		type AccountId = u64;
		type Lookup = IdentityLookup<u64>;
		type Header = Header;
		type Event = ();
		type WeightMultiplierUpdate = ();
		type MaximumBlockWeight = MaximumBlockWeight;
		type MaximumBlockLength = MaximumBlockLength;
		type AvailableBlockRatio = AvailableBlockRatio;
		type BlockHashCount = BlockHashCount;
	}
	parameter_types! {
		pub const ExistentialDeposit: u64 = 0;
		pub const TransferFee: u64 = 0;
		pub const CreationFee: u64 = 0;
		pub const TransactionBaseFee: u64 = 0;
		pub const TransactionByteFee: u64 = 0;
	}
	impl balances::Trait for Test {
		type Balance = u64;
		type OnFreeBalanceZero = ();
		type OnNewAccount = ();
		type Event = ();
		type TransactionPayment = ();
		type TransferPayment = ();
		type DustRemoval = ();
		type ExistentialDeposit = ExistentialDeposit;
		type TransferFee = TransferFee;
		type CreationFee = CreationFee;
		type TransactionBaseFee = TransactionBaseFee;
		type TransactionByteFee = TransactionByteFee;
		type WeightToFee = ();
	}
	parameter_types! {
		pub const MinimumPeriod: u64 = 1000;
	}
	impl timestamp::Trait for Test {
		type Moment = u64;
		type OnTimestampSet = ();
		type MinimumPeriod = MinimumPeriod;
	}

	parameter_types! {
		pub const ClaimSecondsPerExp: u64 = 50; // 10s
		pub const ClaimCurrencyPerSecond: u64 = 10; // $1 per 2000s
		pub const ClaimSecondsMax: u64 = 100;
	}

	impl Trait for Test {
		type Event = ();
		type Currency = balances::Module<Test>;
		type ClaimSecondsPerExp = ClaimSecondsPerExp;
		type ClaimCurrencyPerSecond = ClaimCurrencyPerSecond;
		type ClaimSecondsMax = ClaimSecondsMax;
	}
	type OwnedKittiesTest = OwnedKitties<Test>;

	// This function basically just builds a genesis storage key/value store according to
	// our desired mockup.
	pub fn new_test_ext() -> runtime_io::TestExternalities<Blake2Hasher> {
		system::GenesisConfig::default().build_storage::<Test>().unwrap().into()
	}

	// TODO: test claim exp & claim seconds max
	// TODO: test claim currency & claim seconds max

	#[test]
	fn owned_kitties_can_append_values() {
		with_externalities(&mut new_test_ext(), || {
			OwnedKittiesList::<Test>::append(&0, 1.into());

			assert_eq!(
				OwnedKittiesTest::get(&(0, None)),
				Some(KittyLinkedItem {
					prev: Some(1.into()),
					next: Some(1.into()),
				})
			);

			assert_eq!(
				OwnedKittiesTest::get(&(0, Some(1.into()))),
				Some(KittyLinkedItem { prev: None, next: None })
			);

			OwnedKittiesList::<Test>::append(&0, 2.into());

			assert_eq!(
				OwnedKittiesTest::get(&(0, None)),
				Some(KittyLinkedItem {
					prev: Some(2.into()),
					next: Some(1.into()),
				})
			);

			assert_eq!(
				OwnedKittiesTest::get(&(0, Some(1.into()))),
				Some(KittyLinkedItem {
					prev: None,
					next: Some(2.into()),
				})
			);

			assert_eq!(
				OwnedKittiesTest::get(&(0, Some(2.into()))),
				Some(KittyLinkedItem {
					prev: Some(1.into()),
					next: None,
				})
			);

			OwnedKittiesList::<Test>::append(&0, 3.into());

			assert_eq!(
				OwnedKittiesTest::get(&(0, None)),
				Some(KittyLinkedItem {
					prev: Some(3.into()),
					next: Some(1.into()),
				})
			);

			assert_eq!(
				OwnedKittiesTest::get(&(0, Some(1.into()))),
				Some(KittyLinkedItem {
					prev: None,
					next: Some(2.into()),
				})
			);

			assert_eq!(
				OwnedKittiesTest::get(&(0, Some(2.into()))),
				Some(KittyLinkedItem {
					prev: Some(1.into()),
					next: Some(3.into()),
				})
			);

			assert_eq!(
				OwnedKittiesTest::get(&(0, Some(3.into()))),
				Some(KittyLinkedItem {
					prev: Some(2.into()),
					next: None,
				})
			);
		});
	}

	#[test]
	fn owned_kitties_can_remove_values() {
		with_externalities(&mut new_test_ext(), || {
			OwnedKittiesList::<Test>::append(&0, 1.into());
			OwnedKittiesList::<Test>::append(&0, 2.into());
			OwnedKittiesList::<Test>::append(&0, 3.into());

			OwnedKittiesList::<Test>::remove(&0, 2.into());

			assert_eq!(
				OwnedKittiesTest::get(&(0, None)),
				Some(KittyLinkedItem {
					prev: Some(3.into()),
					next: Some(1.into()),
				})
			);

			assert_eq!(
				OwnedKittiesTest::get(&(0, Some(1.into()))),
				Some(KittyLinkedItem {
					prev: None,
					next: Some(3.into()),
				})
			);

			assert_eq!(OwnedKittiesTest::get(&(0, Some(2.into()))), None);

			assert_eq!(
				OwnedKittiesTest::get(&(0, Some(3.into()))),
				Some(KittyLinkedItem {
					prev: Some(1.into()),
					next: None,
				})
			);

			OwnedKittiesList::<Test>::remove(&0, 1.into());

			assert_eq!(
				OwnedKittiesTest::get(&(0, None)),
				Some(KittyLinkedItem {
					prev: Some(3.into()),
					next: Some(3.into()),
				})
			);

			assert_eq!(OwnedKittiesTest::get(&(0, Some(1.into()))), None);

			assert_eq!(OwnedKittiesTest::get(&(0, Some(2.into()))), None);

			assert_eq!(
				OwnedKittiesTest::get(&(0, Some(3.into()))),
				Some(KittyLinkedItem { prev: None, next: None })
			);

			OwnedKittiesList::<Test>::remove(&0, 3.into());

			assert_eq!(
				OwnedKittiesTest::get(&(0, None)),
				Some(KittyLinkedItem { prev: None, next: None })
			);

			assert_eq!(OwnedKittiesTest::get(&(0, Some(1.into()))), None);

			assert_eq!(OwnedKittiesTest::get(&(0, Some(2.into()))), None);

			assert_eq!(OwnedKittiesTest::get(&(0, Some(2.into()))), None);
		});
	}
}
