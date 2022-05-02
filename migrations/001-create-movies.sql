create table movies (
  id serial primary key not null,
  title text not null,
  released_date date not null,
  rating int not null
);
