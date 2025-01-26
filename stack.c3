module stack(<Type>);

const usz INITIAL_SIZE = 16;

struct Stack
{
	Type* items;
	usz capacity;
	usz count;
	Allocator allocator;
}

fn Stack* Stack.new_init(&self, Allocator allocator = allocator::heap()) @inline
{
	self.items = null;
	self.capacity = 0;
	self.count = 0;
	self.allocator = allocator;

	self.ensure_size(INITIAL_SIZE);

	return self;
}

macro Stack* Stack.temp_init(&self) => self.new_init(allocator::temp());

fn Stack new(Allocator allocator = allocator::heap()) @inline
{
	Stack res;
	res.new_init(allocator);
	return res;
}

macro Stack temp_new() => new(allocator::temp());

fn void Stack.free(&self)
{
	if (self.items != null)
	{
		allocator::free(self.allocator, self.items);
	}
	*self = {};
}


fn void Stack.push(&self, Type item) @inline
{
	self.ensure_size(self.count + 1);
	self.items[self.count++] = item;
}

macro Type Stack.pop(&self) => self.try_pop()!!;

fn Type! Stack.try_pop(&self) @inline
{
	if (self.count == 0) return SearchResult.MISSING?;
	return self.items[--self.count];
}

macro Type Stack.peek(&self) => self.try_peek()!!;

fn Type! Stack.try_peek(&self) @inline
{
	if (self.count == 0) return SearchResult.MISSING?;
	return self.items[self.count];
}

macro usz Stack.len(&self) @operator(len) => self.count;

<*
 @require ind < self.count
*>
macro Type Stack.at(&self, usz ind) @operator([]) => self.items[ind];

<*
 @require ind < self.count
*>
macro Type* Stack.ref_at(&self, usz ind) @operator(&[]) => &self.items[ind];

module stack(<Type>) @private;

import std::math;

macro void Stack.ensure_size(&self, usz size)
{
	if (self.capacity < size)
	{
		usz next = math::next_power_of_2(size);
		Type* items = allocator::realloc(self.allocator, self.items, Type.sizeof * next);
		assert(items != null);

		self.items = items;
		self.capacity = next;
	}
}
