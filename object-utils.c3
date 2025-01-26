module pdf::object::utils;

import std::collections::list;

distinct RevArrayIterator = PdfArray;

fn RevArrayIterator PdfArray.rev_iter(self)
{
	return (RevArrayIterator)self;
}

fn usz RevArrayIterator.len(self) @inline @operator(len) => ((PdfArray)self).list.len();

fn PdfObject RevArrayIterator.at(self, usz ind) @inline @operator([])
{
	List(<PdfObject>) list = ((PdfArray)self).list;
	return list[(list.len() - 1) - ind];
}
