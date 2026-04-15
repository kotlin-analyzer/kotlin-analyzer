@Decorated 
Foo.(
    @Serializable List<out Bar?>, 
    @NonNullable () -> Baz, 
    dynamic, 
    String & (Generic<in T>),
    ((a: A, b: B) -> C)?
) -> @[Ret Res] Map<in Key, Value?>?