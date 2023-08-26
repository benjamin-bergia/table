{application, 'table', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['table','table_app','table_factory','table_heir','table_owner','table_sup']},
	{registered, [table_sup]},
	{applications, [kernel,stdlib]},
	{mod, {table_app, []}},
	{env, []}
]}.