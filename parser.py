import re

class Parser:
    pass


class NodeMeta(type):
    pass


class Node(metaclass=NodeMeta):
    def __init__(self):
        pass

    def __or__(self, other):
        return None  # Some new Node that does some stuff


class Literal(Node):
    def __init__(self, regex):
        super().__init__()
        self.pattern = re.compile(regex)
        self.regex = regex
    def __str__(self):
        return str(self.regex)


class OrNode(Node):
    def __init__(self, *nodes):
        super().__init__()
        self.nodes = list(nodes)

if __name__ == "__main__":
    x = Literal(r"\+")
    print(x)