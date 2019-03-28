extern crate datetime;

use std::borrow::Borrow;
use std::hash::{Hash, Hasher};
use std::rc::{Rc, Weak};
use std::cell::{Cell, RefCell, Ref};
use std::collections::HashSet;
use datetime::Instant;
use std::collections::LinkedList;

type IdType = u32;
pub type MoneyType = i64;
pub type TagId = IdType;
pub type AccountId = IdType;

pub const TAG_FEE_NAME: &'static str = "sys\nfee";
pub const TAG_TRANSFER_NAME: &'static str = "sys\ntransfer";

#[derive(Eq)]
pub struct Currency {
    name: String,
    course: MoneyType,
}

pub struct Account {
    id: AccountId,
    name: String,
    description: String,
    currency: Rc<Currency>,

    /// Whether this account is additive. In additive accounts you may delete individual
    /// transactions. Only final sum is used. This value stores this sum if the account
    /// is additive.
    ///
    /// When account is converted into additive then it is enough to
    /// find last transaction and get it's total money value.
    /// When account is converted into normal then all transactions must be recounted
    /// to get right values of money totals into them to correct the mistakes formed from
    /// deleting transactions when account was additive.
    additive: Option<MoneyType>,

    /// Tag that marks transactions to relate to this account.
    tag: Rc<Tag>,

    last_transaction: RefCell<Rc<Transaction>>,
}

/// Filter provides a view for transactions with given set of tags.
/// Transactions that contain all the tags listed in filter are viewed.
pub struct Filter {
    name: String,
    description: String,
    tags: HashSet<Rc<Tag>>,
}

pub struct Transaction {

    name: Rc<RefCell<String>>,

    description: Rc<RefCell<String>>,

    /// How much money was deposited or spent.
    money_flow: MoneyType,

    /// Total amount of money after transaction.
    total: MoneyType,

    group: Cell<Option<Rc<TransactionGroup>>>,

    /// Account in which this transaction was carried out.
    account: Weak<Account>,

    /// Tags for this transaction. If it contains some tags of a business then it is not necessary
    /// to list them here too.
    tags: HashSet<Rc<Tag>>,

    time_instant: Instant,

    prev: RefCell<Option<Rc<Transaction>>>,
    next: RefCell<Option<Weak<Transaction>>>,
}

/// Group of transaction that were carried out simultaneously and are related to
/// each other. For example transfer between two accounts are separated into two transactions:
/// deposit to one account and withdraw from another. If canceled, both should be
/// undone.
#[derive(Clone)]
pub struct TransactionGroup {
    transactions: RefCell<LinkedList<Weak<Transaction>>>,
}

/// Plan for some thing with predefined amount of money which can be spent in different ways.
pub struct Plan {
    /// Amount of money reserved for planning.
    plan: MoneyType,

    /// Account that stores information about this plan and transactions related to it.
    account: Rc<Account>,
}

pub struct Tag {
    id: TagId,
    name: String,
    description: String,

    /// Tag registry that contains this tag.
    tag_registry: Rc<TagRegistry>,

    /// Transactions that use this tag automatically derive this set of tags.
    derived_tags: HashSet<Rc<Tag>>,
}

/// Registry that contains all tags of the system in use.
pub struct TagRegistry {
    set: RefCell<HashSet<Rc<Tag>>>,
    count: Cell<TagId>,

    fee_tag: Rc<Tag>,
    transfer_tag: Rc<Tag>,
}

/// Information about object that the trading operations are made with. It can be some shop,
/// oil station, service provider etc.
pub struct Business {
    name: String,
    description: String,

    /// Tag that marks transaction made with this business.
    tag: Rc<Tag>,
}

impl PartialEq for Currency {

    fn eq(&self, other: &Self) -> bool {
        self.course == other.course
    }
}

impl PartialEq for Account {

    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Account {}

impl Account {

    pub fn newrc(
        id: AccountId,
        name: String,
        description: String,
        currency: Rc<Currency>,
        tag: Rc<Tag>,
    ) -> Rc<Self> {
        let account = Account {
            id,
            name,
            description,
            currency,
            tag,
            additive: None,
            last_transaction: RefCell::new(unsafe { std::mem::uninitialized() }),
        };
        let rc = Rc::new(account);
        let mut transaction
            = Rc::new(Transaction::initial(Rc::downgrade(&rc)));

        // Initialize uninit value.
        unsafe { std::ptr::write(rc.last_transaction.as_ptr(), transaction); }
        rc
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn description(&self) -> &str {
        self.description.as_str()
    }

    pub fn currency(&self) -> &Rc<Currency> {
        &self.currency
    }

    pub fn tag(&self) -> &Rc<Tag> {
        &self.tag
    }

    pub fn last_transaction(&self) -> Rc<Transaction> {
        self.last_transaction.borrow().clone()
    }

    pub fn is_additive(&self) -> bool {
        self.additive.is_some()
    }

    /// Total money currently on the account.
    pub fn total(&self) -> MoneyType {
        match self.additive {
            Some(t) => t,
            None => self.last_transaction.borrow().total
        }
    }

    pub fn carry_out_new_transaction(
        &self,
        name: Rc<RefCell<String>>,
        description: Rc<RefCell<String>>,
        money_flow: MoneyType,
        group: Option<Rc<TransactionGroup>>,
        tags: HashSet<Rc<Tag>>
    ) -> Rc<Transaction> {
        let mut transaction = Transaction::new_for_previous(
            self.last_transaction.borrow().clone(),
            name,
            description,
            money_flow,
            tags,
        );

        if let Some(g) = group {
            // In curly braces to drop mutable reference before taking Weak which will cause
            // panic.
            { Rc::get_mut(&mut transaction).unwrap().group = Cell::new(Some(g.clone())); }

            // Append this transaction to a group.
            g.transactions.borrow_mut().push_back(Rc::downgrade(&transaction));
        }

        // Update last transaction pointer.
        self.last_transaction.replace(transaction.clone());

        transaction
    }

    /// Transfer money to other account.
    pub fn transfer_to(
        &self,
        other: &Account,
        flow: MoneyType,
        mut tags: HashSet<Rc<Tag>>,
        name: Rc<RefCell<String>>,
        description: Rc<RefCell<String>>
    ) -> Rc<TransactionGroup> {
        let group = TransactionGroup::new();
        let group = Rc::new(group);

        tags.insert(self.tag.tag_registry().transfer_tag().clone());

        let currency_coef = self.currency.conversion_coef(&other.currency);

        self.carry_out_new_transaction(
            name.clone(),
            description.clone(),
            -flow,
            Some(group.clone()),
            tags.clone()
        );
        other.carry_out_new_transaction(
            name,
            description,
            (flow as f64 * currency_coef).floor() as _,
            Some(group.clone()),
            tags
        );

        group
    }

    /// Transfer money to other account and subtract fee from this account.
    pub fn transfer_with_fixed_fee(
        &self,
        other: &Account,
        flow: MoneyType,
        fee: MoneyType,
        mut tags: HashSet<Rc<Tag>>,
        transaction_name: String,
        transaction_description: String
    ) -> Rc<TransactionGroup> {
        let name = Rc::new(RefCell::new(transaction_name));
        let description = Rc::new(RefCell::new(transaction_description));

        let group_rc = self.transfer_to(
            other,
            flow,
            tags.clone(),
            name.clone(),
            description.clone()
        );

        // Insert fee tag and create fee transaction.
        tags.insert(other.tag().tag_registry().fee_tag().clone());
        self.carry_out_new_transaction(
            name,
            description,
            -fee,
            Some(group_rc.clone()),
            tags,
        );

        group_rc
    }

    /// Transfer money to other account and suntract fee in percents from actual money transferred.
    ///
    /// # Panics
    /// Fee percent should not be negative.
    pub fn transfer_with_percent_fee(
        &self,
        other: &Account,
        flow: MoneyType,
        fee: f64,
        tags: HashSet<Rc<Tag>>,
        transaction_name: String,
        transaction_description: String,
    ) -> Rc<TransactionGroup> {
        assert!(fee >= 0.);
        let fee = (flow as f64 * fee).floor() as MoneyType;
        self.transfer_with_fixed_fee(
            other,
            flow,
            fee,
            tags,
            transaction_name,
            transaction_description
        )
    }
}

impl Filter {

    /// Filter transactions to fit all tags listed in the filter.
    /// The filtering starts from the given transaction.
    /// Filtered result is located in new LinkedList.
    pub fn collect(&self, first: Rc<Transaction>) -> LinkedList<Rc<Transaction>> {
        let mut result = LinkedList::new();

        let mut cur = Some(first);
        while cur.is_some() {
            let lcur = cur.unwrap();
            if self.fits_filter(&lcur) {
                result.push_back(lcur.clone());
            }

            cur = lcur.prev.borrow().clone();
        }

        result
    }

    pub fn fits_filter(&self, transaction: &Transaction) -> bool {
        self.tags.is_subset(&transaction.collect_all_tags())
    }

    pub fn name(&self) -> &str {
        &self.name.as_str()
    }

    pub fn description(&self) -> &str {
        &self.description.as_str()
    }

    pub fn tags(&self) -> &HashSet<Rc<Tag>> {
        &self.tags
    }
}

impl Transaction {

    /// Create new transaction based on money value from this previous transaction.
    /// The transaction will relate to the same account.
    fn new_for_previous(
        prev: Rc<Transaction>,
        name: Rc<RefCell<String>>,
        description: Rc<RefCell<String>>,
        flow: MoneyType,
        tags: HashSet<Rc<Tag>>,
    ) -> Rc<Self> {
        let t = Transaction {
            name,
            description,
            total: prev.total + flow,
            group: Cell::new(None),
            money_flow: flow,
            account: prev.account.clone(),
            tags,
            time_instant: Instant::now(),
            prev: RefCell::new(Some(prev.clone())),
            next: RefCell::new(None),
        };

        let rc = Rc::new(t);

        // Update previous transaction's next pointer.
        prev.next.replace(Some(Rc::downgrade(&rc)));

        rc
    }

    /// Create initial account transaction. It contains 0 money with 0 flow and empty tag list.
    pub fn initial(account: Weak<Account>) -> Self {
        Transaction {
            name: Rc::new(RefCell::new(String::new())),
            description: Rc::new(RefCell::new(String::new())),
            total: 0,
            group: Cell::new(None),
            money_flow: 0,
            account,
            tags: HashSet::new(),
            time_instant: Instant::now(),
            prev: RefCell::new(None),
            next: RefCell::new(None),
        }
    }

    pub fn contains_tag(&self, tag: &Tag) -> bool {
        if self.tags.is_empty() {
            return false;
        }

        // Check account tags.
        if *self.account.upgrade().unwrap().tag == *tag {
            return true;
        }
        if self.account.upgrade().unwrap().tag.collect_all_derived_tags().contains(tag) {
            return true;
        }

        let contains = self.tags.contains(tag);
        if !contains {
            let tag_registry = self.tags.iter().next().unwrap().tag_registry();
            let subtags = tag_registry.collect_derived_tags_for_set(&self.tags);
            subtags.contains(tag)
        } else {
            true
        }
    }

    /// Create list of all tags, including derived ones.
    pub fn collect_all_tags(&self) -> HashSet<Rc<Tag>> {
        let tag_registry = self.tag_registry();

        // Get account tag and it's derived tags.
        let mut tags
            = self.account.upgrade().unwrap().tag.collect_all_derived_tags();
        tags.insert(self.account.upgrade().unwrap().tag.clone());

        if tag_registry.is_none() {
            // No direct tags.
        } else {
            // Insert direct tags.
            tags.reserve(self.tags.len());
            for tag in &self.tags {
                tags.insert(tag.clone());
            }

            // Add subtags of direct derived tags.
            let tag_registry = tag_registry.unwrap();
            let subtags = tag_registry.collect_derived_tags_for_set(&self.tags);
            tags.reserve(subtags.len());
            for t in subtags {
                tags.insert(t.clone());
            }
        }

        tags
    }

    pub fn tag_registry(&self) -> Option<Rc<TagRegistry>> {
        if self.tags.is_empty() {
            None
        } else {
            Some(self.tags.iter().next().unwrap().tag_registry().clone())
        }
    }

    pub fn money_flow(&self) -> MoneyType {
        self.money_flow
    }

    pub fn money_total(&self) -> MoneyType {
        self.total
    }

    pub fn time_instant(&self) -> &Instant {
        &self.time_instant
    }

    pub fn name(&self) -> Ref<String> {
        let b: &RefCell<_> = self.name.borrow();
        b.borrow()
    }

    pub fn description(&self) -> Ref<String> {
        let b: &RefCell<_> = self.name.borrow();
        b.borrow()
    }

    pub fn name_rc(&self) -> &Rc<RefCell<String>> {
        &self.name
    }

    pub fn description_rc(&self) -> &Rc<RefCell<String>> {
        &self.description
    }

    /// Set new time instant. Old one is returned.
    pub fn set_time_instant(&mut self, mut instant: Instant) -> Instant {
        std::mem::swap(&mut self.time_instant, &mut instant);
        instant
    }

    /// Cancel this transaction as if it wasn't carried out.
    pub fn cancel(&self) {
        let money_flow = self.money_flow;
        let next = &self.next;
        let prev = &self.prev;

        // Check whether this is initial transaction. If so, do not do anything. Initial
        // cannot be removed.
        if prev.borrow().is_none() {
            return;
        }

        // Check whether this is not the last transaction of the account.
        if let Some(next) = &*next.borrow() {
            // Update next transaction pointer of the previous transaction.
            let prev = prev.borrow().clone().unwrap();
            prev.next.replace(Some(next.clone()));

            // Update previous transaction pointer of the next transaction.
            next.upgrade().unwrap().prev.replace(Some(prev));
        } else {
            // Update last transaction pointer of the account.
            let prev = prev.borrow().clone().unwrap();
            self.account.upgrade().unwrap().last_transaction.replace(prev);
        }

        // Undo changes in consequent transactions.
        let mut cur = next.as_ptr();
        unsafe {
            while let Some(i) = &*cur {
                let rc = i.upgrade().unwrap();
                let iptr = Rc::into_raw(rc.clone()) as *mut Transaction;
                let i = &mut *iptr;
                i.total -= money_flow;
                Rc::from_raw(iptr);

                cur = rc.next.as_ptr();
            }
        }
    }
}

impl TransactionGroup {

    pub fn transactions(&self) -> Ref<LinkedList<Weak<Transaction>>> {
        self.transactions.borrow()
    }

    pub fn new() -> Self {
        TransactionGroup {
            transactions: RefCell::new(LinkedList::new())
        }
    }
}

impl Plan {

    pub fn plan_sum(&self) -> MoneyType {
        self.plan
    }

    pub fn account(&self) -> &Rc<Account> {
        &self.account
    }
}

impl Currency {

    /// Create new currency with given name and course.
    ///
    /// # Panic
    /// Course should not be 0 or else method causes panic.
    pub fn new(name: String, course: MoneyType) -> Self {
        assert_ne!(course, 0);
        Currency {
            name,
            course,
        }
    }

    /// Conversion coefficient to apply to convert value of money from one currency to another.
    /// If current currency has higher course than given then coefficient is lower then 1.
    pub fn conversion_coef(&self, into: &Currency) -> f64 {
        let this = self.course as f64;
        let into = into.course as f64;
        into / this
    }

    /// Convert one currency to another.
    pub fn convert(&self, into: &Currency, value: isize) -> f64 {
        self.conversion_coef(into) * (value as f64)
    }

    pub fn course(&self) -> MoneyType {
        self.course
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

impl Tag {

    pub fn newrc(
        name: String,
        description: String,
        tag_registry: Rc<TagRegistry>,
        derived_tags: HashSet<Rc<Tag>>,
    ) -> Rc<Self> {
        let tag = Tag {
            id: 0, // will be later assigned by registry to a valid ID.
            name,
            description,
            tag_registry: tag_registry.clone(),
            derived_tags,
        };

        tag_registry.insert_new_tag(tag)
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn description(&self) -> &str {
        self.description.as_str()
    }

    /// Collect all derived tags for this tag.
    pub fn collect_all_derived_tags(&self) -> HashSet<Rc<Tag>> {
        self.tag_registry.collect_derived_tags(&self)
    }

    pub fn tag_registry(&self) -> Rc<TagRegistry> {
        self.tag_registry.clone()
    }
}

impl Hash for Tag {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Eq for Tag {}

impl TagRegistry {

    pub fn newrc() -> Rc<Self> {
        let tr = TagRegistry {
            set: Default::default(),
            count: Cell::new(0),

            fee_tag: unsafe { std::mem::uninitialized() },
            transfer_tag: unsafe { std::mem::uninitialized() },
        };
        let tr = Rc::new(tr);

        let fee_tag = Tag::newrc(
            String::from(TAG_FEE_NAME),
            String::new(),
            tr.clone(),
            HashSet::new()
        );
        let transfer_tag = Tag::newrc(
            String::from(TAG_TRANSFER_NAME),
            String::new(),
            tr.clone(),
            HashSet::new()
        );

        let ptr = Rc::into_raw(tr);
        unsafe {
            let r = &mut *(ptr as *mut TagRegistry);
            std::ptr::write(&mut r.fee_tag as *mut Rc<Tag>, fee_tag.clone());
            r.set.borrow_mut().insert(fee_tag);

        }
        let tr = unsafe {
            let r = &mut *(ptr as *mut TagRegistry);
            std::ptr::write(&mut r.transfer_tag as *mut Rc<Tag>, transfer_tag.clone());
            r.set.borrow_mut().insert(transfer_tag);

            Rc::from_raw(ptr)
        };

        tr
    }

    /// Collect all derived tags for this tag.
    pub fn collect_derived_tags(&self, tag: &Tag) -> HashSet<Rc<Tag>> {
        let mut result = HashSet::new();
        result.reserve(tag.derived_tags.len() + 1);
        for tag in &tag.derived_tags {
            result.insert(tag.clone());
            let subtags = self.collect_derived_tags(&tag);
            result.reserve(subtags.len());
            for tag in subtags {
                result.insert(tag);
            }
        }
        result
    }

    pub fn collect_derived_tags_for_set(&self, tags: &HashSet<Rc<Tag>>) -> HashSet<Rc<Tag>> {
        let mut result = HashSet::new();

        for tag in tags {
            let tags = tag.collect_all_derived_tags();
            result.reserve(tags.len());
            for tag in tags {
                result.insert(tag);
            }
        }

        result
    }

    /// Insert given tag into the registry and assign it a correct unique id.
    pub fn insert_new_tag(&self, mut tag: Tag) -> Rc<Tag> {
        tag.id = self.count.get();
        self.count.set(self.count.get() + 1);
        let rc = Rc::new(tag);

        let set = unsafe { &mut *self.set.as_ptr() };

        set.insert(rc.clone());
        rc
    }

    /// Get tag that marks fee transactions.
    pub fn fee_tag(&self) -> &Rc<Tag> {
        &self.fee_tag
    }

    /// Get tag that marks transfer from one account to another.
    pub fn transfer_tag(&self) -> &Rc<Tag> {
        &self.transfer_tag
    }
}

impl PartialEq for Tag {

    fn eq(&self, other: &Tag) -> bool {
        self.id == other.id
    }
}

impl Business {

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn description(&self) -> &str {
        self.description.as_str()
    }

    pub fn tag(&self) -> &Rc<Tag> {
        &self.tag
    }
}

#[cfg(test)]
mod tests {
    use crate::Account;
    use crate::Currency;
    use crate::Tag;
    use std::rc::Rc;
    use std::collections::HashSet;
    use crate::TagRegistry;
    use crate::Transaction;
    use std::cell::RefCell;
    use std::env::var;

    #[test]
    fn collect_tags() {
        let tag_reg = TagRegistry::newrc();

        let account_derived_tag = Tag::newrc(
            String::from("account derived tag"),
            String::new(),
            tag_reg.clone(),
            HashSet::new()
        );
        let mut account_derived_tag_set = HashSet::new();
        account_derived_tag_set.insert(account_derived_tag.clone());
        let account_tag = Tag::newrc(
            String::from("account tag"),
            String::new(),
            tag_reg.clone(),
            account_derived_tag_set
        );

        let account = Account::newrc(
            0,
            String::from("Account"),
            String::new(),
            Rc::new(Currency {
                name: String::new(),
                course: 1000,
            }),
            account_tag.clone()
        );

        let mut trans = Transaction::initial(Rc::downgrade(&account));

        let trans_subtag = Tag::newrc(
            String::from("trans subtag"),
            String::new(),
            tag_reg.clone(),
            HashSet::new()
        );
        let mut trans_subtag_set = HashSet::new();
        trans_subtag_set.insert(trans_subtag.clone());

        let trans_tag = Tag::newrc(
            String::from("trans tag"),
            String::new(),
            tag_reg.clone(),
            trans_subtag_set
        );
        let mut trans_tag_set = HashSet::new();
        trans_tag_set.insert(trans_tag.clone());

        trans.tags = trans_tag_set;

        let expected_tags = {
            let mut h = HashSet::new();
            h.insert(account_derived_tag);
            h.insert(account_tag);
            h.insert(trans_subtag);
            h.insert(trans_tag);
            h
        };

        let tags = trans.collect_all_tags();

        assert_eq!(expected_tags.difference(&tags).count(), 0);
    }

    #[test]
    fn carry_out_transaction() {
        let tag_registry = TagRegistry::newrc();
        let account = Account::newrc(
            0,
            String::from(""),
            String::from(""),
            Rc::new(Currency::new(String::from(""), 1000)),
            Tag::newrc(
                String::from(""), String::from(""),
                tag_registry,
                HashSet::new()
            ),
        );

        account.carry_out_new_transaction(
            Default::default(),
            Default::default(),
            1000,
            None,
            HashSet::new()
        );
        account.carry_out_new_transaction(
            Default::default(),
            Default::default(),
            -500,
            None,
            HashSet::new()
        );

        assert_eq!(account.total(), 500);
    }

    #[test]
    /// Cancel transaction by removing the middle transaction.
    fn cancel_transaction_middle() {
        let tag_registry = TagRegistry::newrc();
        let account = Account::newrc(
            0,
            String::from(""),
            String::from(""),
            Rc::new(Currency::new(String::from(""), 1000)),
            Tag::newrc(
                String::from(""), String::from(""),
                tag_registry,
                HashSet::new()
            ),
        );

        let a = account.carry_out_new_transaction(
            Default::default(),
            Default::default(),
            1000,
            None,
            HashSet::new()
        );
        let b = account.carry_out_new_transaction(
            Default::default(),
            Default::default(),
            -500,
            None,
            HashSet::new()
        );
        let c = account.carry_out_new_transaction(
            Default::default(),
            Default::default(),
            -250,
            None,
            HashSet::new()
        );
        let d = account.carry_out_new_transaction(
            Default::default(),
            Default::default(),
            -100,
            None,
            HashSet::new()
        );

        b.cancel();
        assert_eq!(c.money_total(), 750);
        assert_eq!(d.money_total(), 650);
        assert_eq!(account.total(), 650);

        d.cancel();
        assert_eq!(account.total(), 750);
    }
}
