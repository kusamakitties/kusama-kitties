type Balance = u128;
type Moment = u64;

pub const DOLLAR: Balance = 1_000_000_000_000;

pub const CLAIM_SECONDS_PER_EXP: Moment = 1000 * 10; // 10s
pub const CLAIM_CURRENCY_PER_SECOND: Balance = DOLLAR / 1000 / 2000; // $1 per 2000s
pub const CLAIM_SECONDS_MAX: Moment = 1000 * 60 * 60 * 24 * 7; // 7days

pub const CAPTURE_KITTY_COST: Balance = DOLLAR * 50;

pub const BREED_KITTY_BASE_COST: Balance = DOLLAR * 30;
pub const BREED_KITTY_COST_PER_GENERATION: Balance = DOLLAR * 5;
