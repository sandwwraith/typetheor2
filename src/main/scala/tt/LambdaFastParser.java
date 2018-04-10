package tt;

import java.util.HashMap;
import java.util.Map;

import static java.lang.Character.*;

public class LambdaFastParser {
    private final String str;
    private final char[] s;
    private int pos;
    private Map<String, String> map;
    private int counter;

    private int lamCounter;

    public LambdaFastParser(String s) {
        this.str = s;
        this.s = (s.trim() + "#").toCharArray();
    }

    public Lambda parse() {
        pos = 0;
        map = new HashMap<>();
        counter = 0;
        lamCounter = 0;
        Lambda ex = exp();
        return ex;
    }

    private void skip(String t) {
        while (isWhitespace(s[pos])) {
            ++pos;
        }
        if (str.startsWith(t, pos)) {
            pos += t.length();
        }
    }

    private Lambda exp() {
        if (s[pos] == '\\') {
            return lam();
        } else {
            Lambda res = atom();
            Lambda e;
            skip("");
            while ((e = atom()) != null) {
                res = new Application(res, e);
                skip("");
            }
            return res;
        }
    }

    private Abstraction lam() {
        skip("\\");
        Variable var = var();
        skip(".");
        skip("");

        String read = var.name();
        String oldName = map.getOrDefault(read, "?");
        ++counter;
        String newName = "var" + counter;
        map.put(var.name(), newName);
        var = new Variable(newName);
        Lambda exp = exp();
        if (oldName.equals("?")) {
            map.remove(read);
        } else {
            map.put(read, oldName);
        }

        return new Abstraction(var, exp);
    }

    private Lambda atom() {
        skip("");
        if (s[pos] == '(') {
            skip("(");
            Lambda e = exp();
            skip(")");
            return e;
        } else {
            Variable var = var();
            if (var == null) {
                return null;
            }
            if (map.containsKey(var.name())) {
                var = new Variable(map.get(var.name()));
            }
            return var;
        }
    }

    private Variable var() {
        skip("");
        StringBuilder ret = new StringBuilder();
        int startPos = pos;
        while (isLowerCase(s[pos]) || isDigit(s[pos]) || s[pos] == '\'') {
            ret.append(s[pos++]);
        }
        String name = ret.toString();
        if (name.isEmpty()) {
            return null;
        } else {
            boolean free = !map.containsKey(name);
            if (startPos > 0 && s[startPos - 1] == '\\') {
                free = false;
            } else if (map.containsKey(name)) {
                name = map.get(name);
            }
            return new Variable(name);
        }
    }
}
