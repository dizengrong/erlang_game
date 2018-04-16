# This is an auto-generated Django model module.
# You'll have to do the following manually to clean this up:
#     * Rearrange models' order
#     * Make sure each model has one field with primary_key=True
# Feel free to rename the models, but don't rename db_table values or field names.
#
# Also note: You'll have to insert the output of 'django-admin.py sqlcustom [appname]'
# into your database.
from __future__ import unicode_literals

from django.db import models


class TabAccount(models.Model):
    id = models.IntegerField(primary_key=True)
    role_id = models.IntegerField(unique=True)
    account = models.CharField(max_length=255L, unique=True)
    role_name = models.CharField(max_length=255L)
    last_login_time = models.IntegerField()
    class Meta:
        db_table = 'tab_account'

class TabAchievement(models.Model):
    id = models.IntegerField(primary_key=True)
    role_id = models.IntegerField()
    achieve_id = models.IntegerField()
    achieve_level = models.IntegerField()
    data = models.CharField(max_length=255L)
    class Meta:
        db_table = 'tab_achievement'

class TabDragon(models.Model):
    id = models.IntegerField(primary_key=True)
    role_id = models.IntegerField(unique=True)
    dragon_id = models.IntegerField()
    level = models.IntegerField()
    exp = models.IntegerField()
    magic = models.IntegerField()
    rage = models.IntegerField()
    class Meta:
        db_table = 'tab_dragon'

class TabFightAttr(models.Model):
    id = models.IntegerField(primary_key=True)
    role_id = models.IntegerField(unique=True)
    hero_id = models.IntegerField()
    leadership = models.IntegerField()
    attack = models.IntegerField()
    defence = models.IntegerField()
    intel = models.IntegerField()
    magic = models.IntegerField()
    max_magic = models.IntegerField()
    rage = models.IntegerField()
    max_rage = models.IntegerField()
    move_speed = models.IntegerField()
    class Meta:
        db_table = 'tab_fight_attr'

class TabItem(models.Model):
    id = models.IntegerField(primary_key=True)
    role_id = models.IntegerField()
    item_id = models.IntegerField()
    type_id = models.IntegerField()
    location = models.IntegerField()
    embe_items = models.CharField(max_length=255L)
    amount = models.IntegerField()
    start_time = models.IntegerField()
    end_time = models.IntegerField()
    morale = models.IntegerField()
    used_times = models.IntegerField()
    attr = models.CharField(max_length=1024L)
    is_bind = models.IntegerField()
    firm_exp = models.IntegerField()
    firm_lv = models.IntegerField()
    endurance = models.IntegerField()
    colour = models.IntegerField()
    class Meta:
        db_table = 'tab_item'

class TabLogGold(models.Model):
    id = models.IntegerField(primary_key=True)
    role_id = models.IntegerField(unique=True)
    old_gold = models.IntegerField()
    new_gold = models.IntegerField()
    in_or_use = models.IntegerField()
    log_descript = models.CharField(max_length=255L)
    class Meta:
        db_table = 'tab_log_gold'

class TabLogItem(models.Model):
    id = models.IntegerField(primary_key=True)
    role_id = models.IntegerField(unique=True)
    item_typeid = models.IntegerField()
    item_name = models.CharField(max_length=255L)
    log_descript = models.CharField(max_length=255L)
    class Meta:
        db_table = 'tab_log_item'

class TabMagicBook(models.Model):
    id = models.IntegerField(primary_key=True)
    role_id = models.IntegerField()
    magic_id = models.IntegerField()
    book_id = models.IntegerField()
    magic_level = models.IntegerField()
    has_learned = models.IntegerField(null=True, blank=True)
    class Meta:
        db_table = 'tab_magic_book'

class TabMagicData(models.Model):
    id = models.IntegerField(primary_key=True)
    role_id = models.IntegerField(unique=True)
    max_scroll = models.IntegerField()
    magic_crystal = models.IntegerField()
    class Meta:
        db_table = 'tab_magic_data'

class TabRoleChat(models.Model):
    id = models.IntegerField(primary_key=True)
    role_id = models.IntegerField(unique=True)
    no_speak = models.IntegerField()
    class Meta:
        db_table = 'tab_role_chat'

class TabRoleData(models.Model):
    id = models.IntegerField(primary_key=True)
    role_id = models.IntegerField(unique=True)
    role_level = models.IntegerField()
    role_exp = models.IntegerField()
    reserve_troops = models.IntegerField()
    bag_capacity = models.IntegerField()
    class Meta:
        db_table = 'tab_role_data'

class TabRolePos(models.Model):
    id = models.IntegerField(primary_key=True)
    role_id = models.IntegerField(unique=True)
    cur_mapid = models.IntegerField()
    cur_x = models.IntegerField()
    cur_y = models.IntegerField()
    dir = models.IntegerField()
    class Meta:
        db_table = 'tab_role_pos'

class TabRune(models.Model):
    id = models.IntegerField(primary_key=True)
    role_id = models.IntegerField(unique=True)
    str_rune = models.IntegerField()
    will_rune = models.IntegerField()
    magic_rune = models.IntegerField()
    str_rune_skills = models.CharField(max_length=255L)
    will_rune_skills = models.CharField(max_length=255L)
    magic_rune_skills = models.CharField(max_length=255L)
    class Meta:
        db_table = 'tab_rune'

class TabTroops(models.Model):
    id = models.IntegerField(primary_key=True)
    role_id = models.IntegerField()
    troops_id = models.IntegerField()
    troops_amount = models.IntegerField()
    place = models.IntegerField()
    level = models.IntegerField()
    exp = models.IntegerField()
    class Meta:
        db_table = 'tab_troops'


