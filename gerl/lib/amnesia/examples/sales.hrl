
-record (customer, {
	id = null,
	customer_code = null,
	name = null,
	address = null,
	email = []}).

-record (product, {
	id = null,
	product_code = null,
	description = null,
	price = 1.0}).

-record (orders, {
	id = null,
	order_number = null,
	order_date = {2007,7,12},
	customer = null}).

-record (order_line, {
	id = null,
	orders = null,
	product = null,
	quantity = null}).

