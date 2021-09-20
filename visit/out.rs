#[allow(unused_variables)]
pub fn fold_arc_enum<V: ?Sized + Fold>(_visitor: &mut V, n: Arc<Enum>) -> Arc<Enum> {
    {
        n
    }
}
#[allow(unused_variables)]
pub fn fold_arc_enums<V: ?Sized + Fold>(_visitor: &mut V, n: Arc<Vec<Enum>>) -> Arc<Vec<Enum>> {
    {
        n
    }
}
#[allow(unused_variables)]
pub fn fold_arc_item<V: ?Sized + Fold>(_visitor: &mut V, n: Arc<Item>) -> Arc<Item> {
    {
        n
    }
}
#[allow(unused_variables)]
pub fn fold_arc_items<V: ?Sized + Fold>(_visitor: &mut V, n: Arc<Vec<Item>>) -> Arc<Vec<Item>> {
    {
        n
    }
}
#[allow(unused_variables)]
pub fn fold_enum<V: ?Sized + Fold>(_visitor: &mut V, n: Enum) -> Enum {
    {
        match n {
            Enum::Item { 0: _0 } => {
                let _0 = _visitor.fold_arc_item(_0);
                return Enum::Item { 0: _0 };
            }
            Enum::Items { 0: _0 } => {
                let _0 = _visitor.fold_arc_items(_0);
                return Enum::Items { 0: _0 };
            }
            Enum::Enum { 0: _0 } => {
                let _0 = _visitor.fold_arc_enum(_0);
                return Enum::Enum { 0: _0 };
            }
            Enum::Enums { 0: _0 } => {
                let _0 = _visitor.fold_arc_enums(_0);
                return Enum::Enums { 0: _0 };
            }
        }
    }
}
#[allow(unused_variables)]
pub fn fold_enums<V: ?Sized + Fold>(_visitor: &mut V, n: Vec<Enum>) -> Vec<Enum> {
    {
        global_visit::util::move_map::MoveMap::move_map(n, |v| _visitor.fold_enum(v))
    }
}
#[allow(unused_variables)]
pub fn fold_item<V: ?Sized + Fold>(_visitor: &mut V, n: Item) -> Item {
    {
        match n {
            Item { item, ref_to_enum } => {
                let item = _visitor.fold_opt_arc_item(item);
                let ref_to_enum = _visitor.fold_opt_arc_enum(ref_to_enum);
                return Item { item, ref_to_enum };
            }
        }
    }
}
#[allow(unused_variables)]
pub fn fold_items<V: ?Sized + Fold>(_visitor: &mut V, n: Vec<Item>) -> Vec<Item> {
    {
        global_visit::util::move_map::MoveMap::move_map(n, |v| _visitor.fold_item(v))
    }
}
#[allow(unused_variables)]
pub fn fold_opt_arc_enum<V: ?Sized + Fold>(
    _visitor: &mut V,
    n: Option<Arc<Enum>>,
) -> Option<Arc<Enum>> {
    {
        match n {
            Some(n) => Some(_visitor.fold_arc_enum(n)),
            None => None,
        }
    }
}
#[allow(unused_variables)]
pub fn fold_opt_arc_item<V: ?Sized + Fold>(
    _visitor: &mut V,
    n: Option<Arc<Item>>,
) -> Option<Arc<Item>> {
    {
        match n {
            Some(n) => Some(_visitor.fold_arc_item(n)),
            None => None,
        }
    }
}
pub trait Fold {
    #[allow(unused_variables)]
    fn fold_arc_enum(&mut self, n: Arc<Enum>) -> Arc<Enum> {
        fold_arc_enum(self, n)
    }
    #[allow(unused_variables)]
    fn fold_arc_enums(&mut self, n: Arc<Vec<Enum>>) -> Arc<Vec<Enum>> {
        fold_arc_enums(self, n)
    }
    #[allow(unused_variables)]
    fn fold_arc_item(&mut self, n: Arc<Item>) -> Arc<Item> {
        fold_arc_item(self, n)
    }
    #[allow(unused_variables)]
    fn fold_arc_items(&mut self, n: Arc<Vec<Item>>) -> Arc<Vec<Item>> {
        fold_arc_items(self, n)
    }
    #[allow(unused_variables)]
    fn fold_enum(&mut self, n: Enum) -> Enum {
        fold_enum(self, n)
    }
    #[allow(unused_variables)]
    fn fold_enums(&mut self, n: Vec<Enum>) -> Vec<Enum> {
        fold_enums(self, n)
    }
    #[allow(unused_variables)]
    fn fold_item(&mut self, n: Item) -> Item {
        fold_item(self, n)
    }
    #[allow(unused_variables)]
    fn fold_items(&mut self, n: Vec<Item>) -> Vec<Item> {
        fold_items(self, n)
    }
    #[allow(unused_variables)]
    fn fold_opt_arc_enum(&mut self, n: Option<Arc<Enum>>) -> Option<Arc<Enum>> {
        fold_opt_arc_enum(self, n)
    }
    #[allow(unused_variables)]
    fn fold_opt_arc_item(&mut self, n: Option<Arc<Item>>) -> Option<Arc<Item>> {
        fold_opt_arc_item(self, n)
    }
}
impl<'a, V> Fold for &'a mut V
where
    V: ?Sized + Fold,
{
    fn fold_arc_enum(&mut self, n: Arc<Enum>) -> Arc<Enum> {
        (**self).fold_arc_enum(n)
    }
    fn fold_arc_enums(&mut self, n: Arc<Vec<Enum>>) -> Arc<Vec<Enum>> {
        (**self).fold_arc_enums(n)
    }
    fn fold_arc_item(&mut self, n: Arc<Item>) -> Arc<Item> {
        (**self).fold_arc_item(n)
    }
    fn fold_arc_items(&mut self, n: Arc<Vec<Item>>) -> Arc<Vec<Item>> {
        (**self).fold_arc_items(n)
    }
    fn fold_enum(&mut self, n: Enum) -> Enum {
        (**self).fold_enum(n)
    }
    fn fold_enums(&mut self, n: Vec<Enum>) -> Vec<Enum> {
        (**self).fold_enums(n)
    }
    fn fold_item(&mut self, n: Item) -> Item {
        (**self).fold_item(n)
    }
    fn fold_items(&mut self, n: Vec<Item>) -> Vec<Item> {
        (**self).fold_items(n)
    }
    fn fold_opt_arc_enum(&mut self, n: Option<Arc<Enum>>) -> Option<Arc<Enum>> {
        (**self).fold_opt_arc_enum(n)
    }
    fn fold_opt_arc_item(&mut self, n: Option<Arc<Item>>) -> Option<Arc<Item>> {
        (**self).fold_opt_arc_item(n)
    }
}
impl<V> Fold for Box<V>
where
    V: ?Sized + Fold,
{
    fn fold_arc_enum(&mut self, n: Arc<Enum>) -> Arc<Enum> {
        (**self).fold_arc_enum(n)
    }
    fn fold_arc_enums(&mut self, n: Arc<Vec<Enum>>) -> Arc<Vec<Enum>> {
        (**self).fold_arc_enums(n)
    }
    fn fold_arc_item(&mut self, n: Arc<Item>) -> Arc<Item> {
        (**self).fold_arc_item(n)
    }
    fn fold_arc_items(&mut self, n: Arc<Vec<Item>>) -> Arc<Vec<Item>> {
        (**self).fold_arc_items(n)
    }
    fn fold_enum(&mut self, n: Enum) -> Enum {
        (**self).fold_enum(n)
    }
    fn fold_enums(&mut self, n: Vec<Enum>) -> Vec<Enum> {
        (**self).fold_enums(n)
    }
    fn fold_item(&mut self, n: Item) -> Item {
        (**self).fold_item(n)
    }
    fn fold_items(&mut self, n: Vec<Item>) -> Vec<Item> {
        (**self).fold_items(n)
    }
    fn fold_opt_arc_enum(&mut self, n: Option<Arc<Enum>>) -> Option<Arc<Enum>> {
        (**self).fold_opt_arc_enum(n)
    }
    fn fold_opt_arc_item(&mut self, n: Option<Arc<Item>>) -> Option<Arc<Item>> {
        (**self).fold_opt_arc_item(n)
    }
}
impl<V> Fold for ::global_visit::Optional<V>
where
    V: Fold,
{
    fn fold_arc_enum(&mut self, n: Arc<Enum>) -> Arc<Enum> {
        if self.enabled {
            self.visitor.fold_arc_enum(n)
        } else {
            n
        }
    }
    fn fold_arc_enums(&mut self, n: Arc<Vec<Enum>>) -> Arc<Vec<Enum>> {
        if self.enabled {
            self.visitor.fold_arc_enums(n)
        } else {
            n
        }
    }
    fn fold_arc_item(&mut self, n: Arc<Item>) -> Arc<Item> {
        if self.enabled {
            self.visitor.fold_arc_item(n)
        } else {
            n
        }
    }
    fn fold_arc_items(&mut self, n: Arc<Vec<Item>>) -> Arc<Vec<Item>> {
        if self.enabled {
            self.visitor.fold_arc_items(n)
        } else {
            n
        }
    }
    fn fold_enum(&mut self, n: Enum) -> Enum {
        if self.enabled {
            self.visitor.fold_enum(n)
        } else {
            n
        }
    }
    fn fold_enums(&mut self, n: Vec<Enum>) -> Vec<Enum> {
        if self.enabled {
            self.visitor.fold_enums(n)
        } else {
            n
        }
    }
    fn fold_item(&mut self, n: Item) -> Item {
        if self.enabled {
            self.visitor.fold_item(n)
        } else {
            n
        }
    }
    fn fold_items(&mut self, n: Vec<Item>) -> Vec<Item> {
        if self.enabled {
            self.visitor.fold_items(n)
        } else {
            n
        }
    }
    fn fold_opt_arc_enum(&mut self, n: Option<Arc<Enum>>) -> Option<Arc<Enum>> {
        if self.enabled {
            self.visitor.fold_opt_arc_enum(n)
        } else {
            n
        }
    }
    fn fold_opt_arc_item(&mut self, n: Option<Arc<Item>>) -> Option<Arc<Item>> {
        if self.enabled {
            self.visitor.fold_opt_arc_item(n)
        } else {
            n
        }
    }
}
impl<A, B> Fold for ::global_visit::Either<A, B>
where
    A: Fold,
    B: Fold,
{
    fn fold_arc_enum(&mut self, n: Arc<Enum>) -> Arc<Enum> {
        match self {
            global_visit::Either::Left(v) => v.fold_arc_enum(n),
            global_visit::Either::Right(v) => v.fold_arc_enum(n),
        }
    }
    fn fold_arc_enums(&mut self, n: Arc<Vec<Enum>>) -> Arc<Vec<Enum>> {
        match self {
            global_visit::Either::Left(v) => v.fold_arc_enums(n),
            global_visit::Either::Right(v) => v.fold_arc_enums(n),
        }
    }
    fn fold_arc_item(&mut self, n: Arc<Item>) -> Arc<Item> {
        match self {
            global_visit::Either::Left(v) => v.fold_arc_item(n),
            global_visit::Either::Right(v) => v.fold_arc_item(n),
        }
    }
    fn fold_arc_items(&mut self, n: Arc<Vec<Item>>) -> Arc<Vec<Item>> {
        match self {
            global_visit::Either::Left(v) => v.fold_arc_items(n),
            global_visit::Either::Right(v) => v.fold_arc_items(n),
        }
    }
    fn fold_enum(&mut self, n: Enum) -> Enum {
        match self {
            global_visit::Either::Left(v) => v.fold_enum(n),
            global_visit::Either::Right(v) => v.fold_enum(n),
        }
    }
    fn fold_enums(&mut self, n: Vec<Enum>) -> Vec<Enum> {
        match self {
            global_visit::Either::Left(v) => v.fold_enums(n),
            global_visit::Either::Right(v) => v.fold_enums(n),
        }
    }
    fn fold_item(&mut self, n: Item) -> Item {
        match self {
            global_visit::Either::Left(v) => v.fold_item(n),
            global_visit::Either::Right(v) => v.fold_item(n),
        }
    }
    fn fold_items(&mut self, n: Vec<Item>) -> Vec<Item> {
        match self {
            global_visit::Either::Left(v) => v.fold_items(n),
            global_visit::Either::Right(v) => v.fold_items(n),
        }
    }
    fn fold_opt_arc_enum(&mut self, n: Option<Arc<Enum>>) -> Option<Arc<Enum>> {
        match self {
            global_visit::Either::Left(v) => v.fold_opt_arc_enum(n),
            global_visit::Either::Right(v) => v.fold_opt_arc_enum(n),
        }
    }
    fn fold_opt_arc_item(&mut self, n: Option<Arc<Item>>) -> Option<Arc<Item>> {
        match self {
            global_visit::Either::Left(v) => v.fold_opt_arc_item(n),
            global_visit::Either::Right(v) => v.fold_opt_arc_item(n),
        }
    }
}
pub trait FoldWith<V: Fold> {
    fn fold_with(self, v: &mut V) -> Self;
    #[doc = r" Visit children nodes of self with `v`"]
    fn fold_children_with(self, v: &mut V) -> Self;
}
impl<V, T> FoldWith<V> for Box<T>
where
    V: Fold,
    T: 'static + FoldWith<V>,
{
    fn fold_with(self, v: &mut V) -> Self {
        global_visit::util::map::Map::map(self, |value| value.fold_with(v))
    }
    #[doc = r" Visit children nodes of self with `v`"]
    fn fold_children_with(self, v: &mut V) -> Self {
        global_visit::util::map::Map::map(self, |value| value.fold_children_with(v))
    }
}
impl<V: Fold> FoldWith<V> for Arc<Enum> {
    fn fold_with(self, v: &mut V) -> Self {
        v.fold_arc_enum(self)
    }
    fn fold_children_with(self, v: &mut V) -> Self {
        fold_arc_enum(v, self)
    }
}
impl<V: Fold> FoldWith<V> for Arc<Vec<Enum>> {
    fn fold_with(self, v: &mut V) -> Self {
        v.fold_arc_enums(self)
    }
    fn fold_children_with(self, v: &mut V) -> Self {
        fold_arc_enums(v, self)
    }
}
impl<V: Fold> FoldWith<V> for Arc<Item> {
    fn fold_with(self, v: &mut V) -> Self {
        v.fold_arc_item(self)
    }
    fn fold_children_with(self, v: &mut V) -> Self {
        fold_arc_item(v, self)
    }
}
impl<V: Fold> FoldWith<V> for Arc<Vec<Item>> {
    fn fold_with(self, v: &mut V) -> Self {
        v.fold_arc_items(self)
    }
    fn fold_children_with(self, v: &mut V) -> Self {
        fold_arc_items(v, self)
    }
}
impl<V: Fold> FoldWith<V> for Enum {
    fn fold_with(self, v: &mut V) -> Self {
        v.fold_enum(self)
    }
    fn fold_children_with(self, v: &mut V) -> Self {
        fold_enum(v, self)
    }
}
impl<V: Fold> FoldWith<V> for Vec<Enum> {
    fn fold_with(self, v: &mut V) -> Self {
        v.fold_enums(self)
    }
    fn fold_children_with(self, v: &mut V) -> Self {
        fold_enums(v, self)
    }
}
impl<V: Fold> FoldWith<V> for Item {
    fn fold_with(self, v: &mut V) -> Self {
        v.fold_item(self)
    }
    fn fold_children_with(self, v: &mut V) -> Self {
        fold_item(v, self)
    }
}
impl<V: Fold> FoldWith<V> for Vec<Item> {
    fn fold_with(self, v: &mut V) -> Self {
        v.fold_items(self)
    }
    fn fold_children_with(self, v: &mut V) -> Self {
        fold_items(v, self)
    }
}
impl<V: Fold> FoldWith<V> for Option<Arc<Enum>> {
    fn fold_with(self, v: &mut V) -> Self {
        v.fold_opt_arc_enum(self)
    }
    fn fold_children_with(self, v: &mut V) -> Self {
        fold_opt_arc_enum(v, self)
    }
}
impl<V: Fold> FoldWith<V> for Option<Arc<Item>> {
    fn fold_with(self, v: &mut V) -> Self {
        v.fold_opt_arc_item(self)
    }
    fn fold_children_with(self, v: &mut V) -> Self {
        fold_opt_arc_item(v, self)
    }
}
#[allow(unused_variables)]
pub fn visit_arc_enum<'ast, V: ?Sized + Visit<'ast>>(
    _visitor: &mut V,
    n: &'ast Arc<Enum>,
    _parent: AstNode<'ast>,
) {
    {
        _visitor.visit_enum(n, _parent)
    }
}
#[allow(unused_variables)]
pub fn visit_arc_enums<'ast, V: ?Sized + Visit<'ast>>(
    _visitor: &mut V,
    n: &'ast Arc<Vec<Enum>>,
    _parent: AstNode<'ast>,
) {
    {
        _visitor.visit_enums(n, _parent)
    }
}
#[allow(unused_variables)]
pub fn visit_arc_item<'ast, V: ?Sized + Visit<'ast>>(
    _visitor: &mut V,
    n: &'ast Arc<Item>,
    _parent: AstNode<'ast>,
) {
    {
        _visitor.visit_item(n, _parent)
    }
}
#[allow(unused_variables)]
pub fn visit_arc_items<'ast, V: ?Sized + Visit<'ast>>(
    _visitor: &mut V,
    n: &'ast Arc<Vec<Item>>,
    _parent: AstNode<'ast>,
) {
    {
        _visitor.visit_items(n, _parent)
    }
}
#[allow(unused_variables)]
pub fn visit_enum<'ast, V: ?Sized + Visit<'ast>>(
    _visitor: &mut V,
    n: &'ast Enum,
    _parent: AstNode<'ast>,
) {
    {
        match n {
            Enum::Item { 0: _0 } => {
                _visitor.visit_arc_item(_0, _parent as _);
            }
            Enum::Items { 0: _0 } => {
                _visitor.visit_arc_items(_0, _parent as _);
            }
            Enum::Enum { 0: _0 } => {
                _visitor.visit_arc_enum(_0, _parent as _);
            }
            Enum::Enums { 0: _0 } => {
                _visitor.visit_arc_enums(_0, _parent as _);
            }
        }
    }
}
#[allow(unused_variables)]
pub fn visit_enums<'ast, V: ?Sized + Visit<'ast>>(
    _visitor: &mut V,
    n: &'ast [Enum],
    _parent: AstNode<'ast>,
) {
    {
        n.iter().for_each(|v| _visitor.visit_enum(v, _parent))
    }
}
#[allow(unused_variables)]
pub fn visit_item<'ast, V: ?Sized + Visit<'ast>>(
    _visitor: &mut V,
    n: &'ast Item,
    _parent: AstNode<'ast>,
) {
    {
        match n {
            Item { item, ref_to_enum } => {
                _visitor.visit_opt_arc_item(item.as_ref(), _parent as _);
                _visitor.visit_opt_arc_enum(ref_to_enum.as_ref(), _parent as _);
            }
        }
    }
}
#[allow(unused_variables)]
pub fn visit_items<'ast, V: ?Sized + Visit<'ast>>(
    _visitor: &mut V,
    n: &'ast [Item],
    _parent: AstNode<'ast>,
) {
    {
        n.iter().for_each(|v| _visitor.visit_item(v, _parent))
    }
}
#[allow(unused_variables)]
pub fn visit_opt_arc_enum<'ast, V: ?Sized + Visit<'ast>>(
    _visitor: &mut V,
    n: Option<&'ast Arc<Enum>>,
    _parent: AstNode<'ast>,
) {
    {
        match n {
            Some(n) => _visitor.visit_arc_enum(n, _parent),
            None => {}
        }
    }
}
#[allow(unused_variables)]
pub fn visit_opt_arc_item<'ast, V: ?Sized + Visit<'ast>>(
    _visitor: &mut V,
    n: Option<&'ast Arc<Item>>,
    _parent: AstNode<'ast>,
) {
    {
        match n {
            Some(n) => _visitor.visit_arc_item(n, _parent),
            None => {}
        }
    }
}
pub trait Visit<'ast> {
    #[allow(unused_variables)]
    fn visit_arc_enum(&mut self, n: &'ast Arc<Enum>, _parent: AstNode<'ast>) {
        visit_arc_enum(self, n, _parent)
    }
    #[allow(unused_variables)]
    fn visit_arc_enums(&mut self, n: &'ast Arc<Vec<Enum>>, _parent: AstNode<'ast>) {
        visit_arc_enums(self, n, _parent)
    }
    #[allow(unused_variables)]
    fn visit_arc_item(&mut self, n: &'ast Arc<Item>, _parent: AstNode<'ast>) {
        visit_arc_item(self, n, _parent)
    }
    #[allow(unused_variables)]
    fn visit_arc_items(&mut self, n: &'ast Arc<Vec<Item>>, _parent: AstNode<'ast>) {
        visit_arc_items(self, n, _parent)
    }
    #[allow(unused_variables)]
    fn visit_enum(&mut self, n: &'ast Enum, _parent: AstNode<'ast>) {
        visit_enum(self, n, _parent)
    }
    #[allow(unused_variables)]
    fn visit_enums(&mut self, n: &'ast [Enum], _parent: AstNode<'ast>) {
        visit_enums(self, n, _parent)
    }
    #[allow(unused_variables)]
    fn visit_item(&mut self, n: &'ast Item, _parent: AstNode<'ast>) {
        visit_item(self, n, _parent)
    }
    #[allow(unused_variables)]
    fn visit_items(&mut self, n: &'ast [Item], _parent: AstNode<'ast>) {
        visit_items(self, n, _parent)
    }
    #[allow(unused_variables)]
    fn visit_opt_arc_enum(&mut self, n: Option<&'ast Arc<Enum>>, _parent: AstNode<'ast>) {
        visit_opt_arc_enum(self, n, _parent)
    }
    #[allow(unused_variables)]
    fn visit_opt_arc_item(&mut self, n: Option<&'ast Arc<Item>>, _parent: AstNode<'ast>) {
        visit_opt_arc_item(self, n, _parent)
    }
}
impl<'a, 'ast, V> Visit<'ast> for &'a mut V
where
    V: ?Sized + Visit<'ast>,
{
    fn visit_arc_enum(&mut self, n: &'ast Arc<Enum>, _parent: AstNode<'ast>) {
        (**self).visit_arc_enum(n, _parent)
    }
    fn visit_arc_enums(&mut self, n: &'ast Arc<Vec<Enum>>, _parent: AstNode<'ast>) {
        (**self).visit_arc_enums(n, _parent)
    }
    fn visit_arc_item(&mut self, n: &'ast Arc<Item>, _parent: AstNode<'ast>) {
        (**self).visit_arc_item(n, _parent)
    }
    fn visit_arc_items(&mut self, n: &'ast Arc<Vec<Item>>, _parent: AstNode<'ast>) {
        (**self).visit_arc_items(n, _parent)
    }
    fn visit_enum(&mut self, n: &'ast Enum, _parent: AstNode<'ast>) {
        (**self).visit_enum(n, _parent)
    }
    fn visit_enums(&mut self, n: &'ast [Enum], _parent: AstNode<'ast>) {
        (**self).visit_enums(n, _parent)
    }
    fn visit_item(&mut self, n: &'ast Item, _parent: AstNode<'ast>) {
        (**self).visit_item(n, _parent)
    }
    fn visit_items(&mut self, n: &'ast [Item], _parent: AstNode<'ast>) {
        (**self).visit_items(n, _parent)
    }
    fn visit_opt_arc_enum(&mut self, n: Option<&'ast Arc<Enum>>, _parent: AstNode<'ast>) {
        (**self).visit_opt_arc_enum(n, _parent)
    }
    fn visit_opt_arc_item(&mut self, n: Option<&'ast Arc<Item>>, _parent: AstNode<'ast>) {
        (**self).visit_opt_arc_item(n, _parent)
    }
}
impl<'ast, V> Visit<'ast> for Box<V>
where
    V: ?Sized + Visit<'ast>,
{
    fn visit_arc_enum(&mut self, n: &'ast Arc<Enum>, _parent: AstNode<'ast>) {
        (**self).visit_arc_enum(n, _parent)
    }
    fn visit_arc_enums(&mut self, n: &'ast Arc<Vec<Enum>>, _parent: AstNode<'ast>) {
        (**self).visit_arc_enums(n, _parent)
    }
    fn visit_arc_item(&mut self, n: &'ast Arc<Item>, _parent: AstNode<'ast>) {
        (**self).visit_arc_item(n, _parent)
    }
    fn visit_arc_items(&mut self, n: &'ast Arc<Vec<Item>>, _parent: AstNode<'ast>) {
        (**self).visit_arc_items(n, _parent)
    }
    fn visit_enum(&mut self, n: &'ast Enum, _parent: AstNode<'ast>) {
        (**self).visit_enum(n, _parent)
    }
    fn visit_enums(&mut self, n: &'ast [Enum], _parent: AstNode<'ast>) {
        (**self).visit_enums(n, _parent)
    }
    fn visit_item(&mut self, n: &'ast Item, _parent: AstNode<'ast>) {
        (**self).visit_item(n, _parent)
    }
    fn visit_items(&mut self, n: &'ast [Item], _parent: AstNode<'ast>) {
        (**self).visit_items(n, _parent)
    }
    fn visit_opt_arc_enum(&mut self, n: Option<&'ast Arc<Enum>>, _parent: AstNode<'ast>) {
        (**self).visit_opt_arc_enum(n, _parent)
    }
    fn visit_opt_arc_item(&mut self, n: Option<&'ast Arc<Item>>, _parent: AstNode<'ast>) {
        (**self).visit_opt_arc_item(n, _parent)
    }
}
impl<'ast, V> Visit<'ast> for ::global_visit::Optional<V>
where
    V: Visit<'ast>,
{
    fn visit_arc_enum(&mut self, n: &'ast Arc<Enum>, _parent: AstNode<'ast>) {
        if self.enabled {
            self.visitor.visit_arc_enum(n, _parent)
        }
    }
    fn visit_arc_enums(&mut self, n: &'ast Arc<Vec<Enum>>, _parent: AstNode<'ast>) {
        if self.enabled {
            self.visitor.visit_arc_enums(n, _parent)
        }
    }
    fn visit_arc_item(&mut self, n: &'ast Arc<Item>, _parent: AstNode<'ast>) {
        if self.enabled {
            self.visitor.visit_arc_item(n, _parent)
        }
    }
    fn visit_arc_items(&mut self, n: &'ast Arc<Vec<Item>>, _parent: AstNode<'ast>) {
        if self.enabled {
            self.visitor.visit_arc_items(n, _parent)
        }
    }
    fn visit_enum(&mut self, n: &'ast Enum, _parent: AstNode<'ast>) {
        if self.enabled {
            self.visitor.visit_enum(n, _parent)
        }
    }
    fn visit_enums(&mut self, n: &'ast [Enum], _parent: AstNode<'ast>) {
        if self.enabled {
            self.visitor.visit_enums(n, _parent)
        }
    }
    fn visit_item(&mut self, n: &'ast Item, _parent: AstNode<'ast>) {
        if self.enabled {
            self.visitor.visit_item(n, _parent)
        }
    }
    fn visit_items(&mut self, n: &'ast [Item], _parent: AstNode<'ast>) {
        if self.enabled {
            self.visitor.visit_items(n, _parent)
        }
    }
    fn visit_opt_arc_enum(&mut self, n: Option<&'ast Arc<Enum>>, _parent: AstNode<'ast>) {
        if self.enabled {
            self.visitor.visit_opt_arc_enum(n, _parent)
        }
    }
    fn visit_opt_arc_item(&mut self, n: Option<&'ast Arc<Item>>, _parent: AstNode<'ast>) {
        if self.enabled {
            self.visitor.visit_opt_arc_item(n, _parent)
        }
    }
}
impl<'ast, A, B> Visit<'ast> for ::global_visit::Either<A, B>
where
    A: Visit<'ast>,
    B: Visit<'ast>,
{
    fn visit_arc_enum(&mut self, n: &'ast Arc<Enum>, _parent: AstNode<'ast>) {
        match self {
            global_visit::Either::Left(v) => v.visit_arc_enum(n, _parent),
            global_visit::Either::Right(v) => v.visit_arc_enum(n, _parent),
        }
    }
    fn visit_arc_enums(&mut self, n: &'ast Arc<Vec<Enum>>, _parent: AstNode<'ast>) {
        match self {
            global_visit::Either::Left(v) => v.visit_arc_enums(n, _parent),
            global_visit::Either::Right(v) => v.visit_arc_enums(n, _parent),
        }
    }
    fn visit_arc_item(&mut self, n: &'ast Arc<Item>, _parent: AstNode<'ast>) {
        match self {
            global_visit::Either::Left(v) => v.visit_arc_item(n, _parent),
            global_visit::Either::Right(v) => v.visit_arc_item(n, _parent),
        }
    }
    fn visit_arc_items(&mut self, n: &'ast Arc<Vec<Item>>, _parent: AstNode<'ast>) {
        match self {
            global_visit::Either::Left(v) => v.visit_arc_items(n, _parent),
            global_visit::Either::Right(v) => v.visit_arc_items(n, _parent),
        }
    }
    fn visit_enum(&mut self, n: &'ast Enum, _parent: AstNode<'ast>) {
        match self {
            global_visit::Either::Left(v) => v.visit_enum(n, _parent),
            global_visit::Either::Right(v) => v.visit_enum(n, _parent),
        }
    }
    fn visit_enums(&mut self, n: &'ast [Enum], _parent: AstNode<'ast>) {
        match self {
            global_visit::Either::Left(v) => v.visit_enums(n, _parent),
            global_visit::Either::Right(v) => v.visit_enums(n, _parent),
        }
    }
    fn visit_item(&mut self, n: &'ast Item, _parent: AstNode<'ast>) {
        match self {
            global_visit::Either::Left(v) => v.visit_item(n, _parent),
            global_visit::Either::Right(v) => v.visit_item(n, _parent),
        }
    }
    fn visit_items(&mut self, n: &'ast [Item], _parent: AstNode<'ast>) {
        match self {
            global_visit::Either::Left(v) => v.visit_items(n, _parent),
            global_visit::Either::Right(v) => v.visit_items(n, _parent),
        }
    }
    fn visit_opt_arc_enum(&mut self, n: Option<&'ast Arc<Enum>>, _parent: AstNode<'ast>) {
        match self {
            global_visit::Either::Left(v) => v.visit_opt_arc_enum(n, _parent),
            global_visit::Either::Right(v) => v.visit_opt_arc_enum(n, _parent),
        }
    }
    fn visit_opt_arc_item(&mut self, n: Option<&'ast Arc<Item>>, _parent: AstNode<'ast>) {
        match self {
            global_visit::Either::Left(v) => v.visit_opt_arc_item(n, _parent),
            global_visit::Either::Right(v) => v.visit_opt_arc_item(n, _parent),
        }
    }
}
pub trait VisitWith<'ast, V: Visit<'ast>> {
    fn visit_with(&'ast self, _parent: AstNode<'ast>, v: &mut V);
    #[doc = r" Visit children nodes of self with `v`"]
    fn visit_children_with(&'ast self, v: &mut V);
}
impl<'ast, V, T> VisitWith<'ast, V> for Box<T>
where
    V: Visit<'ast>,
    T: 'static + VisitWith<'ast, V>,
{
    fn visit_with(&'ast self, _parent: AstNode<'ast>, v: &mut V) {
        (**self).visit_with(_parent, v)
    }
    #[doc = r" Visit children nodes of self with `v`"]
    fn visit_children_with(&'ast self, v: &mut V) {
        (**self).visit_children_with(v)
    }
}
impl<'ast, V: Visit<'ast>> VisitWith<'ast, V> for Arc<Enum> {
    fn visit_with(&'ast self, _parent: AstNode<'ast>, v: &mut V) {
        v.visit_arc_enum(self, _parent as _)
    }
    fn visit_children_with(&'ast self, _visitor: &mut V) {
        visit_arc_enum(_visitor, self, _parent)
    }
}
impl<'ast, V: Visit<'ast>> VisitWith<'ast, V> for Arc<Vec<Enum>> {
    fn visit_with(&'ast self, _parent: AstNode<'ast>, v: &mut V) {
        v.visit_arc_enums(self, _parent as _)
    }
    fn visit_children_with(&'ast self, _visitor: &mut V) {
        visit_arc_enums(_visitor, self, _parent)
    }
}
impl<'ast, V: Visit<'ast>> VisitWith<'ast, V> for Arc<Item> {
    fn visit_with(&'ast self, _parent: AstNode<'ast>, v: &mut V) {
        v.visit_arc_item(self, _parent as _)
    }
    fn visit_children_with(&'ast self, _visitor: &mut V) {
        visit_arc_item(_visitor, self, _parent)
    }
}
impl<'ast, V: Visit<'ast>> VisitWith<'ast, V> for Arc<Vec<Item>> {
    fn visit_with(&'ast self, _parent: AstNode<'ast>, v: &mut V) {
        v.visit_arc_items(self, _parent as _)
    }
    fn visit_children_with(&'ast self, _visitor: &mut V) {
        visit_arc_items(_visitor, self, _parent)
    }
}
impl<'ast, V: Visit<'ast>> VisitWith<'ast, V> for Enum {
    fn visit_with(&'ast self, _parent: AstNode<'ast>, v: &mut V) {
        v.visit_enum(self, _parent as _)
    }
    fn visit_children_with(&'ast self, _visitor: &mut V) {
        visit_enum(_visitor, self, _parent)
    }
}
impl<'ast, V: Visit<'ast>> VisitWith<'ast, V> for Vec<Enum> {
    fn visit_with(&'ast self, _parent: AstNode<'ast>, v: &mut V) {
        v.visit_enums(self, _parent as _)
    }
    fn visit_children_with(&'ast self, _visitor: &mut V) {
        visit_enums(_visitor, self, _parent)
    }
}
impl<'ast, V: Visit<'ast>> VisitWith<'ast, V> for Item {
    fn visit_with(&'ast self, _parent: AstNode<'ast>, v: &mut V) {
        v.visit_item(self, _parent as _)
    }
    fn visit_children_with(&'ast self, _visitor: &mut V) {
        visit_item(_visitor, self, _parent)
    }
}
impl<'ast, V: Visit<'ast>> VisitWith<'ast, V> for Vec<Item> {
    fn visit_with(&'ast self, _parent: AstNode<'ast>, v: &mut V) {
        v.visit_items(self, _parent as _)
    }
    fn visit_children_with(&'ast self, _visitor: &mut V) {
        visit_items(_visitor, self, _parent)
    }
}
impl<'ast, V: Visit<'ast>> VisitWith<'ast, V> for Option<Arc<Enum>> {
    fn visit_with(&'ast self, _parent: AstNode<'ast>, v: &mut V) {
        v.visit_opt_arc_enum(self.as_ref(), _parent as _)
    }
    fn visit_children_with(&'ast self, _visitor: &mut V) {
        visit_opt_arc_enum(_visitor, self.as_ref(), _parent)
    }
}
impl<'ast, V: Visit<'ast>> VisitWith<'ast, V> for Option<Arc<Item>> {
    fn visit_with(&'ast self, _parent: AstNode<'ast>, v: &mut V) {
        v.visit_opt_arc_item(self.as_ref(), _parent as _)
    }
    fn visit_children_with(&'ast self, _visitor: &mut V) {
        visit_opt_arc_item(_visitor, self.as_ref(), _parent)
    }
}
