%% ======================= 本gen_cache的主要特性：=========================
%% 1.将sql数据库中对应的表自动转为erlang中与之对应的record，并将数据缓存到ets表中
%% 2.支持将数据插入、更新和删除到ets表中
%% 3.支持定时同步数据到数据库中，并且可以定制缓存执行的间隔时间
%% 4.支持多关键字的数据库表转化到set类型的ets表
%% 5.gen_cache是一个进程，里面可以为多张数据库表进行cache服务
%% 6.多于多关键字的数据库表对应的ets表，会对record中的某一个指定的字段做一个索引

%% ======================== 使用说明：==================================
%% 1.数据库表与ets表一一对应的，包括表名和字段名
%% 2.gen_cache使用amnesia库定义table的方式来规范字段类型，因此会与这个产生耦合
%% 3.每一个数据库表会对应以下的ets表:
%% 		1）表数据ets，会根据数据库表的关键字类型对需要索引的字段作一个转化
%% 		2）脏数据索引表，会对被更新的数据做一个对索引字段的索引表
%% 		3）索引字段的索引表，因为数据库表存在多关键字的情况，然而表数据ets使用的是set类型，因此需要做一个额外的索引

% 例子：
% 数据库表：customer
% +---------------+--------------+------+-----+---------+----------------+
% | Field         | Type         | Null | Key | Default | Extra          |
% +---------------+--------------+------+-----+---------+----------------+
% | id            | int(11)      | NO   | PRI | NULL    | auto_increment |
% | customer_code | int(11)      | NO   | UNI |         |                |
% | name          | varchar(255) | NO   |     |         |                |
% | address       | varchar(255) | NO   |     |         |                |
% | email         | varchar(255) | NO   |     |         |                |
% +---------------+--------------+------+-----+---------+----------------+

% 对应的表数据ets：
% -record(customer, {
% 	id,
% 	customer_code,
% 	name,
% 	address,
% 	email
% }).

% 如果customer为多关键字（id, customer_code），然后表数据ets的索引字段为id，
% 则一条数据库记录：
% +---------------+--------------+-------+---------+------------------+
% | id            | customer_code| name  | address | email            |
% +---------------+--------------+-------+---------+------------------+
% | 1             |   10         | "aaa" | "xxxx"  | "xxx.gmail.com"  |
% +---------------+--------------+-------+---------+------------------+
% 转为gen_cache中的表数据ets数据则会如下：
% #customer{id = {1, 10}, customer_code = 10, name = "aaa", address = "xxxx", email = "xxx.gmail.com"}.
% 但是这一点对使用者来说是透明的，使用者得到的数据将会是：
% #customer{id = 1, customer_code = 10, name = "aaa", address = "xxxx", email = "xxx.gmail.com"}.

%% key_classic已废弃了，现在根据key_fields字段值的长度可知其关键字的类别了
-record(cache_config, {
	index_field = 0, 	%% 索引字段(该字段要位于key_fields中的第一个元素)
	key_classic = 1,	%% 表关键字的类别，1为单关键字（即索引字段），2为两个关键字，以此类推
	key_fields  = []	%% 关键字的所有字段的index和名称[{#xxx_record.xxx_field, "xxx_field"}]
}).