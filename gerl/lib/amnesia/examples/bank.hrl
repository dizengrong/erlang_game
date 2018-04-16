
-record (customer, {
	id = null,
	customer_code = null,
	name = null,
	surname = null,
	is_a_company = false}).

-record (account, {
	id = null,
	account_code = null,
	account_description = null}).

-record (owner, {
	id = null,
	customer = null,
	account = null}).

-record (operation_type, {
	id = null,
	operation_code = null,
	operation_code_description = null}).

-record (operation, {
	id = null,
	account = null,
	operation_type = null,
	operation_description = null,
	operation_date = null,
	amount = null}).

