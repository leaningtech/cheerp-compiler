namespace [[cheerp::genericjs]] client
{
	class SomeClass;
	extern SomeClass* someClassPtr;
}

void func(client::SomeClass** ptr)
{
}

int main()
{
	func(&client::someClassPtr);
}
