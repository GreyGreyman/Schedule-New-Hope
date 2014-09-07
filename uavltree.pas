unit uavltree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

implementation
type
  PNode = ^TNode;
  TNode = record
    key, height: integer;
    left, right: PNode;
  end;
var
  head: PNode;
  buf: integer;

function height(x: PNode): integer;
begin
  if x <> nil then result := x^.height
  else result := 0;
end;

procedure fix_height(x: PNode);
begin
  if x <> nil then x^.height :=
    max(height(x^.right),height(x^.left)) + 1;
end;

function rotate_right(x: PNode): PNode;
  var temp: PNode;
begin
  new(temp);
  temp := x^.left;
  x^.left := temp^.right;
  temp^.right := x;
  fix_height(x);
  fix_height(temp);
  result := temp;
end;

function rotate_left(x: PNode): PNode;
  var temp: PNode;
begin
  new(temp);
  temp := x^.right;
  x^.right := temp^.left;
  temp^.left := x;
  fix_height(temp);
  fix_height(x);
  result := temp;
end;

function deltaheight(x: PNode): integer;
begin
  result := height(x^.right) - height(x^.left);
end;

function balance(x: PNode): PNode;
begin
  fix_height(x);
  if deltaheight(x) = 2 then
    begin
      if deltaheight(x^.right) < 0 then x^.right := rotate_right(x^.right);
      exit(rotate_left(x));
    end;
  if deltaheight(x) = -2 then
    begin
      if deltaheight(x^.left) > 0 then x^.left := rotate_left(x^.left);
      exit(rotate_right(x));
    end;
  result := x;
end;

function insert(x: PNode; key: integer): PNode;
var
  elem: PNode;
begin
  if x = nil then begin
    new(elem);
    elem^.key := key;
    elem^.height := 1;
    elem^.left := nil;
    elem^.right := nil;
    exit(elem);
  end
  else if x^.key = key then exit(x) else
    if x^.key < key then x^.right := insert(x^.right, key)
    else x^.left := insert(x^.left, key);
  result := balance(x);
end;

function min_key(x: PNode): integer;
begin
  while x^.left <> nil do
    x := x^.left;
  result := x^.key;
end;

function delete(x: PNode; key: integer): PNode;
var succ_key: integer;
begin
  if x = nil then exit(nil);
  //поиск
  if x^.key < key then x^.right := delete(x^.right, key)
    else if x^.key > key then x^.left := delete(x^.left, key)
  //есть только левый / правый / нет обоих
    else if x^.right = nil then exit(x^.left)
          else if x^.left = nil then exit(x^.right)
  //есть оба
          else begin
              succ_key := min_key(x^.right);
              delete(x^.right, succ_key);
              x^.key := succ_key;
            end;
  result := balance(x);
end;
end.

