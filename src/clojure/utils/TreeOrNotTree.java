package clojure.utils;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

public class TreeOrNotTree {
    private static final int BINARY_NODE = 3;

    private static final List intoListOrNil(Object o) {
        if (o instanceof List) {
            return (List) o;
        }
        return null;
    }

    private static final boolean isBinaryNode(Object node) {
        return intoListOrNil(node) == null ? false : intoListOrNil(node).size() == BINARY_NODE;
    }

    private static final boolean isCompositeNode(Object node) {
        final List nodes = intoListOrNil(node);
        if (nodes == null) {
            return false;
        }

        for (final Object eachNode : nodes) {
            if (eachNode instanceof List) {
                return true;
            }
        }
        return false;
    }

    private static final boolean containsCompositeNodes(Object node) {
        final List nodes = intoListOrNil(node);
        if (nodes == null) {
            return false;
        }

        for (final Object each : nodes) {
            if (isCompositeNode(each)) {
                return true;
            }
        }
        return false;
    }

    private static final Object first(Object o) {
        final List nodes = intoListOrNil(o);
        return nodes == null || nodes.isEmpty() ? null : nodes.iterator().next();
    }

    private static final Object rest(Object o) {
        return intoListOrNil(o) == null ? null : takeTail(intoListOrNil(o));
    }

    @SuppressWarnings("unchecked")
    private static Object takeTail(List nodes) {
        final Collection restOf = new ArrayList();
        for (int idx = 1; idx < nodes.size(); idx += 1) {
            restOf.add(nodes.get(idx));
        }
        return restOf;
    }

    private static final boolean isBinaryTree(final Object possibleNodes) {
        if (isBinaryNode(possibleNodes) && !containsCompositeNodes(possibleNodes)) {
            return true;
        } else if (isBinaryNode(possibleNodes)) {
            if (isCompositeNode(first(possibleNodes))) {
                return isBinaryTree(first(possibleNodes));
            } else {
                return isBinaryTree(rest(possibleNodes));
            }
        } else {
            return false;
        }
    }

    /* bootstrapping the algorithm */
    public static void main(String[] args) {
        final List binaryTree = Arrays.asList(":a", Arrays.asList(":b", ":c", ":d"), "e");
        final List notABinaryTree = Arrays.asList(":a", Arrays.asList(":b", ":c", ":d"));

        System.out.println("BinaryTree:: " + isBinaryTree(binaryTree));
        System.out.println("Not a BinaryTree:: " + isBinaryTree(notABinaryTree));
    }
}

