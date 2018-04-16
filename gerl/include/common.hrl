%% @doc 本头文件用来包含一些通用的宏定义
%% 不要在这里乱加宏定义，这里的宏定义越少越好
%% 一些可以分门别类的定义应该建立更明确的单独头文件

%% @doc if式的三元表达式
-define(_IF(Expr, TrueExpr, FalseExpr), 
	case Expr of
		true  -> TrueExpr;
		_ 	  -> FalseExpr
	end
).

%% @doc if式的三元表达式，false条件的表达式直接忽略
-define(_IF(Expr, TrueExpr), 
	case Expr of
		true  -> TrueExpr;
		_ 	  -> ignore
	end
).
