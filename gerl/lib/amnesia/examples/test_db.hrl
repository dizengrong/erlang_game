
-record (customer, {
	id = nil,
	name = nil,
	address = nil}).

-record (product, {
	id = nil,
	product_code = nil,
	description = nil,
	price = nil}).

-record (orders, {
	id = nil,
	order_number = nil,
	order_date = nil,
	customer = nil}).

-record (order_line, {
	id = nil,
	orders = nil,
	product = nil,
	quantity = nil}).

