-- Таблица: строение - этаж - кафедра - тип места

create table bmstumap (
    building int, 
    floor int, 
    chair varchar(15), 
    type varchar(25), 
    PRIMARY KEY(building, floor), 
    CHECK(building > 0)
);

insert into bmstumap values (1,1,'FN11','lab'); 
insert into bmstumap values (1,2,'FN1' ,'lectory'); 
insert into bmstumap values (1,3,'FN2' ,'resept'); 
insert into bmstumap values (1,4,'FN11','lab'); 
insert into bmstumap values (1,5,'FN11','resept'); 
insert into bmstumap values (1,6,'FN3' ,'lectory'); 
insert into bmstumap values (1,7,'FN3' ,'lab'); 
insert into bmstumap values (1,8,'IU8' ,'lab'); 
insert into bmstumap values (2,1,'FN11','lectory'); 
insert into bmstumap values (2,2,'IU7' ,'lectory'); 
insert into bmstumap values (2,3,'IU8' ,'lab'); 
insert into bmstumap values (2,4,'IU7' ,'lab'); 
insert into bmstumap values (2,5,'FN2' ,'resept'); 
insert into bmstumap values (2,6,'IU5' ,'resept'); 
insert into bmstumap values (3,1,'IU5' ,'lab'); 
insert into bmstumap values (3,2,'RK5' ,'lectory'); 
insert into bmstumap values (3,3,'RK5' ,'lab'); 
insert into bmstumap values (3,4,'IU3' ,'resept'); 
insert into bmstumap values (3,5,'IU3' ,'lab'); 
insert into bmstumap values (3,6,'FN2' ,'resept'); 
insert into bmstumap values (3,7,'RK5' ,'lectory'); 
insert into bmstumap values (3,8,'RK1' ,'lectory'); 

-------------------------------------------------------------

-- Кафедры, занимающие больше и меньше всех мест
with 
CountAllTypes as (
    select chair, count(type) as amount from bmstumap 
    group by chair
),
MinFinder as (
    select min(amount) as min_amount from CountAllTypes
),
MaxFinder as (
    select max(amount) as max_amount from CountAllTypes
)
select chair, amount from CountAllTypes, MinFinder, MaxFinder 
where min_amount = amount or max_amount = amount 
order by amount;

-- Наибольшее и наименьшее количество конкретных типов мест
with 
CountAllTypes as (
    select type, count(type) as amount from bmstumap 
    group by type
),
MinFinder as (
    select min(amount) as min_amount from CountAllTypes
),
MaxFinder as (
    select max(amount) as max_amount from CountAllTypes
)
select type, amount from CountAllTypes, MinFinder, MaxFinder 
where min_amount = amount or max_amount = amount 
order by amount;

-- Кафедры, где меньше всего уникальных мест
with 
CountAllTypes as (
    select chair, count(distinct type) as amount from bmstumap 
    group by chair
),
MinFinder as (
    select min(amount) as min_amount from CountAllTypes
)
select chair from CountAllTypes, MinFinder 
where min_amount = amount 
order by amount;

