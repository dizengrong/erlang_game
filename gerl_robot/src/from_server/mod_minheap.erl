%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2011 by  <>
%%%-------------------------------------------------------------------
-module(mod_minheap).

-export([
         new_heap/3,
         delete_heap/1,
         is_full/1,
         is_empty/1,
         key_find/2,
         get_top_element/1
        ]).

-export([
         insert/3,
         update/3,
         pop/1,
         delete/2
        ]).

-define(HEAP_CMP_FUNC, heap_cmp_func).
-define(HEAP_INDEX2ELEMENT, heap_index2element).
-define(HEAP_KEY2INDEX, heap_key2index).
-define(HEAP_MAX_SIZE, heap_max_size).
-define(HEAP_SIZE, heap_size).

%% @doc 创建最小堆
new_heap(HeapName, HeapSize, CmpFunc) ->
    set_max_heap_size(HeapName, HeapSize),
    set_heap_size(HeapName, 0),
    set_cmp_func(HeapName, CmpFunc).

%% @doc 删除最小堆
delete_heap(HeapName) ->
    case get_heap_size(HeapName) of
        {error, Reason} ->
            {error, Reason};
        HeapSize ->
            delete_heap2(HeapName, HeapSize, 0)
    end.
delete_heap2(HeapName, HeapSize, HeapSize) ->
    delete_element_by_index(HeapSize, HeapSize),
    delete_heap_size(HeapName),
    delete_max_heap_size(HeapName),
    delete_cmp_func(HeapName);
delete_heap2(HeapName, HeapSize, Count) ->
    delete_element_by_index(HeapName, Count),
    delete_heap2(HeapName, HeapSize, Count+1).

%%获取堆顶元素
get_top_element(HeapName) ->
    case is_empty(HeapName) of
        true->
            {error, not_found};
        false ->
            case get_element_key_by_index(HeapName, 0) of
                {error, Reason} ->
                    {error, Reason};
                {Element, _} ->
                    Element
            end
    end. 

%%插入新的元素
insert(HeapName, Element, Key) ->
    case is_full(HeapName) of
        true ->
            {error, heap_full};
        false ->
            HeapSize = get_heap_size(HeapName),
            set_new_element(HeapName, HeapSize, Element, Key),
            filter_up(HeapName, HeapSize),
            set_heap_size(HeapName, HeapSize+1)
    end.

update(HeapName, Element, Key) ->
    case get_index_by_key(HeapName, Key) of
        {error, Reason} ->
            {error, Reason};
        Index ->
            update(HeapName, Key, Element, Index)
    end.

%%删除堆顶的元素
pop(HeapName) ->
    case is_empty(HeapName) of
        true ->
            {error, not_found};
        false ->
            {TopElement, TopKey} = get_element_key_by_index(HeapName, 0),
            delete_element(HeapName, 0, TopKey),
            LastIndex = get_heap_size(HeapName) - 1,
            set_heap_size(HeapName, LastIndex),
            {LastElement, LastKey} = get_element_key_by_index(HeapName, LastIndex),
            set_new_element(HeapName, 0, LastElement, LastKey),
            filter_down(HeapName, 0),
            TopElement
    end.

%%删除堆中的某一元素然后维护堆
delete(HeapName, Key) ->
    case get_index_by_key(HeapName, Key) of
        {error, Reason} ->
            {error, Reason};
        Index ->
            delete_element(HeapName, Index, Key),
            LastIndex = get_heap_size(HeapName) - 1,
            {LastElement, LastKey} = get_element_key_by_index(HeapName, LastIndex),
            set_new_element(HeapName, Index, LastElement, LastKey),
            set_heap_size(HeapName, LastIndex),
            filter(HeapName, Index, LastElement, LastKey)
    end.

%%判断是否堆满      
is_full(HeapName) ->
    case get_max_heap_size(HeapName) of
        {error, _} ->
            true;
        MaxHeapSize ->
            HeapSize = get_heap_size(HeapName),
            HeapSize >= MaxHeapSize
    end.

%%判断堆是否为空
is_empty(HeapName) ->
    case get_heap_size(HeapName) of
        {error, _} ->
            true;
        HeapSize ->
            HeapSize =:= 0
    end.

key_find(HeapName, Key) ->
    case get_index_by_key(HeapName, Key) of
        {error, Reason} ->
            {error, Reason};
        Index ->
            case get_element_key_by_index(HeapName, Index) of
                {error, Reason} ->
                    {error, Reason};
                {Element, _Key} ->
                    Element
            end
    end.

%%
%%================LOCAL FUCTION=======================
%%
%%跟新堆中元素的值然后重新维护堆
update(HeapName, Key, Element, Index) ->
    set_new_element(HeapName, Index, Element, Key),
    filter(HeapName, Index, Element, Key).

filter(HeapName, Index, Element, _Key) ->
    ParentIndex = trunc((Index - 1) / 2),
    {ParentElement, _ParentKey} = get_element_key_by_index(HeapName, ParentIndex),
    CmpFunc = get_cmp_func(HeapName),
    case CmpFunc(ParentElement, Element) of
        false ->
            %%新的值比父节点小的时候往上跟新
            filter_up(HeapName, Index);
        true ->
            %%新的值比父亲节点大的时候往下跟新
            filter_down(HeapName, Index)
    end.

filter_up(HeapName, Index) ->
    CurrentIndex = Index,
    ParentIndex = trunc((Index - 1) / 2),
    {TargetElement, TargetKey} = get_element_key_by_index(HeapName, CurrentIndex),
    NewCurrentIndex = filter_up2(CurrentIndex, ParentIndex, TargetElement, HeapName),
    set_new_element(HeapName, NewCurrentIndex, TargetElement, TargetKey).

filter_up2(0, _, _, _) ->
    0;
filter_up2(CurrentIndex, ParentIndex, TargetElement, HeapName) ->
    {ParentElement,ParentKey} = get_element_key_by_index(HeapName, ParentIndex),
    CmpFunc = get_cmp_func(HeapName),
    case CmpFunc(ParentElement, TargetElement) of
        true ->
            CurrentIndex;
        false ->
            set_new_element(HeapName, CurrentIndex, ParentElement, ParentKey),
            filter_up2(ParentIndex, trunc((ParentIndex-1)/2), TargetElement, HeapName)
    end.

filter_down(HeapName, Index) ->
    CurrentIndex = Index,
    ChildIndex = 2 * Index + 1,
    {TargetElement, TargetKey} = get_element_key_by_index(HeapName, CurrentIndex),
    HeapSize = get_heap_size(HeapName),
    NewCurrentIndex = filter_down2(CurrentIndex, ChildIndex, TargetElement, HeapSize, HeapName),
    set_new_element(HeapName, NewCurrentIndex, TargetElement, TargetKey).
    

filter_down2(CurrentIndex, ChildIndex, TargetElement, HeapSize, HeapName) ->
    case ChildIndex < HeapSize of
        false ->
            CurrentIndex;
        true ->
            CmpFunc = get_cmp_func(HeapName),
            case ChildIndex + 1 < HeapSize of
                true ->
                    {Element1, _} = get_element_key_by_index(HeapName, ChildIndex+1),
                    {Element2, _} = get_element_key_by_index(HeapName, ChildIndex),
                    case CmpFunc(Element1, Element2) of
                        true ->
                            NewChildIndex = ChildIndex + 1;
                        false ->
                            NewChildIndex = ChildIndex
                    end;
                false ->
                    NewChildIndex = ChildIndex
            end,
            {ChildElement, ChildKey} = get_element_key_by_index(HeapName, NewChildIndex),
            case CmpFunc(TargetElement, ChildElement) of
                true ->
                    CurrentIndex;
                false ->
                    set_new_element(HeapName, CurrentIndex, ChildElement, ChildKey),
                    filter_down2(NewChildIndex, NewChildIndex*2+1, TargetElement, HeapSize, HeapName)
            end
    end.
        
set_cmp_func(HeapName, CmpFunc) ->
    erlang:put({?HEAP_CMP_FUNC, HeapName}, CmpFunc).

get_cmp_func(HeapName) ->
    case erlang:get({?HEAP_CMP_FUNC, HeapName}) of
        undefined ->
            {error, not_found};
        CmpFunc ->
            CmpFunc
    end.

delete_cmp_func(HeapName) ->
    erlang:erase({?HEAP_CMP_FUNC, HeapName}).


delete_element_by_index(HeapName, Index) ->
    case get_element_key_by_index(HeapName, Index) of
        {error, Reason} ->
            {error, Reason};
        {_, Key} ->
            delete_element(HeapName, Index, Key)
    end.

delete_element(HeapName, Index, Key) ->
    erlang:erase({?HEAP_INDEX2ELEMENT, HeapName, Index}),
    erlang:erase({?HEAP_KEY2INDEX, HeapName, Key}).

set_new_element(HeapName, Index, Element, Key) ->
    erlang:put({?HEAP_INDEX2ELEMENT, HeapName, Index}, {Element, Key}),
    erlang:put({?HEAP_KEY2INDEX, HeapName, Key}, Index).

get_element_key_by_index(HeapName, Index) ->
    case erlang:get({?HEAP_INDEX2ELEMENT, HeapName, Index}) of
        undefined ->
            {error, not_found};
        Value ->
            Value
    end.

get_index_by_key(HeapName, Key) ->
    case erlang:get({?HEAP_KEY2INDEX, HeapName, Key}) of
        undefined ->
            {error, not_found};
        Index ->
            Index
    end.

set_heap_size(HeapName, HeapSize) ->
    erlang:put({?HEAP_SIZE, HeapName}, HeapSize).

delete_heap_size(HeapName) ->
    erlang:erase({?HEAP_SIZE, HeapName}).

get_heap_size(HeapName) ->
    case erlang:get({?HEAP_SIZE, HeapName}) of
        undefined ->
            {error, not_found};
        HeapSize ->
            HeapSize
    end.

set_max_heap_size(HeapName, HeapSize) ->
    erlang:put({?HEAP_MAX_SIZE, HeapName}, HeapSize).

get_max_heap_size(HeapName) ->
    case erlang:get({?HEAP_MAX_SIZE, HeapName}) of
        undefined ->
            {error, not_found};
        HeapSize ->
            HeapSize
    end.

delete_max_heap_size(HeapName) ->
    erlang:erase({?HEAP_MAX_SIZE, HeapName}).
