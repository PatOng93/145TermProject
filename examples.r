# Bintree Example

testbintree <- function() {

	mytree <- newbintree()
	push(mytree,5,"mytree")
	push(mytree,4,"mytree")
	push(mytree,6,"mytree")
	push(mytree,6,"mytree")
	push(mytree,7,"mytree")
	push(mytree,8,"mytree")

	print(mytree$data)
	pop(mytree, "mytree")
	print(mytree$data)
	push(mytree,3,"mytree")
	print(mytree$data)

	element <- pop(mytree, "mytree")
	element
}

# Stack example

teststack <- function() {

	mystack <- newstack()
	push(mystack, 5, "mystack")
	push(mystack, 10, "mystack")
	push(mystack, 15, "mystack")
	push(mystack, 20, "mystack")
	push(mystack, 25, "mystack")
	print(mystack$data)
	pop(mystack, "mystack")
	print(mystack$data)
	pop(mystack, "mystack")
	print(mystack$data)
	pop(mystack, "mystack")
	print(mystack$data)
	testVar <- pop(mystack, "mystack")
	print(mystack$data)
	print(testVar)

}

# Queue example

testqueue <- function() {

	myqueue <- newqueue()
	push(myqueue, 5, "myqueue")
	push(myqueue, 10, "myqueue")
	push(myqueue, 15, "myqueue")
	push(myqueue, 20, "myqueue")
	push(myqueue, 25, "myqueue")
	print(myqueue$data)
	pop(myqueue, "myqueue")
	print(myqueue$data)
	pop(myqueue, "myqueue")
	print(myqueue$data)
	pop(myqueue, "myqueue")
	print(myqueue$data)
	testVar <- pop(myqueue, "myqueue")
	print(myqueue$data)
	print(testVar)
}