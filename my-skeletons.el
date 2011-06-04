;;
;; Ruby
;;
(defmacro define-ruby-end-skeleton (name method-string)
  "Creates a skeleton that inserts NAME .. end"
  `(define-skeleton ,name
     "Inserts code"
     > ,method-string " " _ \n "end" >)
  )

(define-ruby-end-skeleton ruby-def-skeleton "def")
(define-ruby-end-skeleton ruby-class-skeleton "class")
(define-ruby-end-skeleton ruby-begin-skeleton "begin")
(define-ruby-end-skeleton ruby-if-skeleton "if")
(define-ruby-end-skeleton ruby-unless-skeleton "unless")
(define-ruby-end-skeleton ruby-while-skeleton "while")
(define-ruby-end-skeleton ruby-case-skeleton "case")

(define-skeleton ruby-block-skeleton
  "Inserts block with args"
  "Block arg(s): "
  "{ | " str " |" \n
  > _ \n
  "}" >)

(defmacro define-ruby-block-skeleton (name method-string)
  "Creates a skeleton that inserts a block with args"
  `(define-skeleton ,name
     "Inserts block with args"
     "Block arg(s): "
     ,method-string " { | " str " | " _ " }" >)
  )

(define-ruby-block-skeleton ruby-collect-skeleton "collect")
(define-ruby-block-skeleton ruby-detect-skeleton "detect")
(define-ruby-block-skeleton ruby-each-skeleton "each")
(define-ruby-block-skeleton ruby-each-with-index-skeleton "each_with_index")
(define-ruby-block-skeleton ruby-find-skeleton "find")
(define-ruby-block-skeleton ruby-find-all-skeleton "find_all")
(define-ruby-block-skeleton ruby-grep-skeleton "grep")
(define-ruby-block-skeleton ruby-map-skeleton "map")
(define-ruby-block-skeleton ruby-reject-skeleton "reject")
(define-ruby-block-skeleton ruby-select-skeleton "select")

(define-skeleton ruby-assert-equal-skeleton
  "Inserts assert_equal()"
  > "assert_equal(" _ ")")

(define-skeleton ruby-assert-not-nil-skeleton
  "Inserts assert_not_nil()"
  > "assert_not_nil(" _ ")")

;;
;; Erb
;;
(define-skeleton erb-eval-skeleton
  "Inserts <% _ %>"
  nil
  "<% " _ " %>")
(define-skeleton erb-print-skeleton
  "Inserts <%= _ %>"
  nil
  "<%= " _ " %>")

;;
;; Rails
;;
(define-skeleton rails-link-to-skeleton
  "Inserts link_to"
  "Link text: "
  "<%= link_to('" str "', :action => '" _ "') %>")

(define-skeleton rails-render-partial-skeleton
  "Inserts render(:partial...)"
  "Partial name: "
  "<%= render(:partial => '" str "', :locals => {" _ "}) %>")

;;
;; Java
;;

;;  > _ ("Type: " str " xx = (" str ")iter.next();" quit) \n                   
(define-skeleton java-iter-skeleton
  "Inserts \"for (Iterator ...) { ... }\" skeleton"
  "List to iterate: "
  > "for (Iterator iter = " str ".iterator(); iter.hasNext(); ) {" \n
  > _ \n
  "}" >)

;;  > _ ("Type: " str " xx = (" str ")iter.next();" quit) \n                   
(define-skeleton java-iter-next-skeleton
  "Inserts \"Type _ = (Type)iter.next();\" skeleton"
  "Type: "
  > str " " _ " = (" str ")iter.next();" \n
  >)

(define-skeleton java-if-skeleton
  "Inserts \"if () { ... }\" skeleton"
  > "if (" _ ") {" \n
  > \n
  "}" >)

(define-skeleton java-else-skeleton
  "Inserts \"else () { ... }\" skeleton"
  > "else (" _ ") {" \n
  > \n
  "}" >)

(define-skeleton java-try-skeleton
  "Inserts \"try { ... } catch (Exception e) {}\" skeleton"
  "Exception to catch: "
  > "try {" \n
  > _ \n
  "}" > \n
  > "catch (" str " e) {" \n
  > \n
  "}" >)
(define-skeleton java-main-skeleton
  "Inserts \"main () { ... }\" skeleton"
  > "public static void main(String[] args) {" \n
  > _ \n
  "}" >)

(define-skeleton java-testcase-with-suite-skeleton
  "Inserts a TestCase class skeleton"
  > "package " (path-to-java-package (buffer-file-name)) ";" \n
  "import junit.framework.TestCase;" \n
  "import junit.framework.TestSuite;" \n
  "import junit.framework.Test;" \n
  \n
  > "public class "
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
  " extends TestCase {" \n
  \n
  > "public static Test suite() {" \n
  > "return new TestSuite("
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
  ".class);" \n
  "}" > \n
  \n
  > "public "
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
  "(String name) {" \n
  > "super(name);" \n
  "}" > \n
  \n
  > "protected void setUp() {" \n
  > _ \n
  "}" > \n
  \n
  > "protected void tearDown() {" \n
  "}" > \n
  \n
  > "public void testDummy() {" \n
  > "assertTrue(true);" \n
  "}" > \n
  "}" \n
  )

(define-skeleton java-testcase-skeleton
  "Inserts a TestCase class skeleton"
  > "package " (path-to-java-package (buffer-file-name)) ";" \n
  "import junit.framework.TestCase;" \n
  \n
  > "public class "
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
  " extends TestCase {" \n
  \n
  > "protected void setUp() {" \n
  > _ \n
  "}" > \n
  \n
  > "protected void tearDown() {" \n
  "}" > \n
  \n
  > "public void testDummy() {" \n
  > "assertTrue(true);" \n
  "}" > \n
  "}" \n
  )

(define-skeleton java-package-skeleton
  "Inserts a package, calculating the proper value for the package name
based on the current directory name."
  > "package " (path-to-java-package (buffer-file-name)) ";" \n
  _
  )

(defun type-from-java-type-and-name (str)
  (car (split-string str " ")))
(defun name-from-java-type-and-name (str)
  (cadr (split-string str " ")))
(defun initial-cap (str)
  (concat (capitalize (substring str 0 1)) (substring str 1)))

(define-skeleton java-accessors-skeleton
  "Inserts an ivar and getter and setter methods. Enter type and name,
for example 'String foo'"
  "Type and name: "
  _ > "private " str ";" \n
  \n
  > "public " (type-from-java-type-and-name str) " get"
  (initial-cap (name-from-java-type-and-name str)) "() { return "
  (name-from-java-type-and-name str) "; }" \n
  > "public void set" (initial-cap (name-from-java-type-and-name str))
  "(" str ") { this." (name-from-java-type-and-name str) " = "
  (name-from-java-type-and-name str) "; }" )

;;
;; Ant
;;
(define-skeleton ant-target-skeleton
  "Inserts a target"
  "Name: "
  > "<target name=\"" str "\">" \n
  > "  " _ \n
  "</target>" >)

;;
;; PostgreSQL
;;
(define-skeleton psql-create-skeleton
  "Inserts a MySQL table create skeleton"
  "Table name: "
  "create table " str \n
  "(" \n
  "  id serial primary key," \n
  > _ \n
  ") type=InnoDB;" \n)

;;
;; MySQL
;;
(define-skeleton mysql-create-skeleton
  "Inserts a MySQL table create skeleton"
  "Table name: "
  "drop table if exists " str ";" \n
  "create table " str " (" \n
  "  id bigint not null auto_increment primary key," \n
  > _ \n
  ") type=InnoDB;" \n)

(define-skeleton mysql-foreign-key-skeleton
  "Inserts a MySQL foreign key declaration"
  "Column name: "
  "foreign key (" str ") references " _ " (id)")

;;
;; Sh-mode
;;
(define-skeleton sh-function-skeleton
  "Inserts a function definition skeleton"
  > "function " _ " {" \n
  > \n
  "}" >)

(define-skeleton sh-for-in-skeleton
  "Inserts a for/do/done statement definition skeleton"
  > "for " _ " in ; do" \n
  > \n
  "done" >)

(define-skeleton sh-iff-skeleton
  "Inserts a if/fi statement definition skeleton"
  > "if [ " _ " ] ; then" \n
  > \n
  "fi" >)

;;
;; PHP-mode
;;
(define-skeleton php-function-skeleton
  "Inserts a function definition skeleton"
  > "function " _ "() {" \n
  > \n
  "}" >)
