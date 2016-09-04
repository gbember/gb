-define(TABLE_KEY_LOCK_TIME,7000).%%锁时间
-define(TABLE_KEY_LOCK_TIMEOUT, 6000).%%锁超时时间


%%缓存参数Key值
-define(TABLE_MAX_MEMORY, 'max_memory').%%最大字节
-define(TABLE_MAX_NUM, 'max_num').%%最大条数
-define(TABLE_CHECK_TIMEOUT, 'check_timeout').%%超时检测间隔
-define(TABLE_KEY_SORT, 'key_sort').%%key sort方式
-define(TABLE_MOD, 'mod').%%表类型 memory | file
%%缓存参数Key值与默认值
-define(TABLE_DEFAULT_OPS, [
    {?TABLE_MAX_MEMORY, 0},
    {?TABLE_MAX_NUM, 0},
    {?TABLE_CHECK_TIMEOUT, 1000},
    {?TABLE_KEY_SORT, 0},
    {?TABLE_MOD, 'gb_table_memory'}
]).
