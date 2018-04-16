import xadmin
from xadmin import views
from models import TabAccount
from xadmin.layout import Main, TabHolder, Tab, Fieldset, Row, Col, AppendedText, Side
from xadmin.plugins.inline import Inline
from xadmin.plugins.batch import BatchChangeAction

class MainDashboard(object):
	widgets = []
	# widgets = [
	# 	[
	# 		{"type": "html", "title": "Test Widget", "content": "<h3> Welcome to Xadmin! </h3><p>Join Online Group: <br/>QQ Qun : 282936295</p>"},
	# 		{"type": "chart", "model": "app.accessrecord", 'chart': 'user_count', 'params': {'_p_date__gte': '2013-01-08', 'p': 1, '_p_date__lt': '2013-01-29'}},
	# 		{"type": "list", "model": "app.host", 'params': {
	# 			'o':'-guarantee_date'}},
	# 	],
	# 	[
	# 		{"type": "qbutton", "title": "Quick Start", "btns": [{'model': Host}, {'model':IDC}, {'title': "Google", 'url': "http://www.google.com"}]},
	# 		{"type": "addform", "model": MaintainLog},
	# 	]
	# ]
xadmin.site.register(views.website.IndexView, MainDashboard)

class BaseSetting(object):
	enable_themes = True
	use_bootswatch = True
xadmin.site.register(views.BaseAdminView, BaseSetting)


class GlobalSetting(object):
	global_search_models = [Host, IDC]
	global_models_icon = {
		Host: 'fa fa-laptop', IDC: 'fa fa-cloud'
	}
	menu_style = 'default'#'accordion'
xadmin.site.register(views.CommAdminView, GlobalSetting)


class TabAccountAdmin(object):
	list_display = ('role_id', 'account', 'role_name', 'last_login_time')
	list_display_links = ('account', 'role_name',)
	wizard_form_list = [
		('First\'s Form', ('name', 'description')),
		('Second Form', ('contact', 'telphone', 'address')),
		('Thread Form', ('customer_id',))
	]

	search_fields = ['account', 'role_name']
	relfield_style = 'fk-ajax'
	reversion_enable = True

	actions = [BatchChangeAction, ]
	batch_fields = ('account', 'role_name')

	  

xadmin.site.register(TabAccount, TabAccountAdmin)
