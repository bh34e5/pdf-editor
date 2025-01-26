module pdf::math;

import pdf::object;

const PdfNumber ZERO = {INTEGER, false, {.integer = 0}};
const PdfNumber ONE  = {INTEGER, false, {.integer = 1}};

struct PdfMatrix
{
	union
	{
		PdfNumber[6] arr;
		struct
		{
			PdfNumber a;
			PdfNumber b;
			PdfNumber c;
			PdfNumber d;
			PdfNumber e;
			PdfNumber f;
		}
	}
}

fn PdfMatrix PdfMatrix.times(self, PdfMatrix other) @inline
{
	PdfMatrix res;
	res.arr[0] = (self.a.times(other.a)).plus(self.b.times(other.c));
	res.arr[1] = (self.a.times(other.b)).plus(self.b.times(other.d));
	res.arr[2] = (self.c.times(other.a)).plus(self.d.times(other.c));
	res.arr[3] = (self.c.times(other.b)).plus(self.d.times(other.d));
	res.arr[4] = (self.e.times(other.a)).plus(self.f.times(other.c)).plus(other.e);
	res.arr[5] = (self.e.times(other.b)).plus(self.f.times(other.d)).plus(other.f);
	return res;
}

fn PdfMatrix identity() @inline => {.arr = {ONE, ZERO, ZERO, ONE, ZERO, ZERO}};

fn PdfNumber PdfNumber.plus(self, PdfNumber other) @inline
{
	if (self.type == INTEGER && other.type == INTEGER)
	{
		return {INTEGER, self.signed || other.signed, {.integer = self.as.integer + other.as.integer}};
	}
	return {REAL, self.signed || other.signed, {.real = self.to_double() + other.to_double()}};
}

macro PdfNumber PdfNumber.minus(self, PdfNumber other) => self.plus(other.negated());

fn PdfNumber PdfNumber.times(self, PdfNumber other) @inline
{
	if (self.type == INTEGER && other.type == INTEGER)
	{
		return {INTEGER, self.signed || other.signed, {.integer = self.as.integer * other.as.integer}};
	}
	return {REAL, self.signed || other.signed, {.real = self.to_double() * other.to_double()}};
}

macro PdfNumber PdfNumber.divide(self, PdfNumber other) => self.times(other.inverted());

fn PdfNumber PdfNumber.negated(self) @inline
{
	switch (self.type)
	{
		case INTEGER: return {INTEGER, true, {.integer = -self.as.integer}};
		case REAL: return {REAL, true, {.real = -self.as.real}};
	}
}

fn PdfNumber PdfNumber.inverted(self) @inline => {REAL, self.signed, {.real = 1.0/self.to_double()}};

module pdf::math @private;

import pdf::object;

macro double PdfNumber.to_double(self)
{
	switch (self.type)
	{
		case INTEGER: return (double)self.as.integer;
		case REAL: return self.as.real;
	}
}
