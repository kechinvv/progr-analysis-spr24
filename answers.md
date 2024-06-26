# Оглавление:

- [Lecture 2](#lec_2)
- [Lecture 3](#lec_3)
- [Lecture 4](#lec_4)
- [Lecture 5](#lec_5)
- [Lecture 6](#lec_6)
- [Lecture 8](#lec_8)

# <a name="lec_2"></a> Lecture 2

### Что будет, если внести тип Bool

- Попробуйте переписать все правила подходящим образом

```
I                       [[I]] = int
B                       [[B]] = Bool
E1 eqOp E2              [[E1]] = [[E2]] ∧ [[E1 eqOp E2]] = Bool
E1 compOp E2            [[E1]] = [[E2]] ∧ [[E1 compOp E2]] = Bool
E1 logOp E2             [[E1]] = [[E2]] = [[E1 logOp E2]] = Bool
E1 op E2                [[E1]] = [[E2]] = [[E1 op E2]] = int
input                   [[input]] = α
X = E                   [[X]] = [[E]]
output E                [[E]] = α
if(E) {S}               [[E]] = Bool
if(E) {S1} else {S2}    [[E]] = Bool
while(E) {S}            [[E]] = Bool
...
```

- Будет ли анализ более полным?

Анализ стал менее полным, так как такие правила не совпадают с валидными конструкциями языка. Например, язык допускает
int в условии if-else или цикла.

- Будет ли анализ более точным?

Точность не изменится. Если анализ успешно выводит тип, то он точно правильный, как и было до этого (soundness).

### Что будет, если в нашу систему ввести тип Array

- Придумайте правила вывода для новых операторов

```
{}                   [[{}]] = α[]
{E1, ..., EN}        [[{E1, ..., EN}]] = [[E1]][] ^ [[E1]] = ... = [[EN]]
A[E]                 [[A[E]]] = α ^ [[E]] = int ^ [[A]] = α[]
A[E1] = E2           [[E2]] = α ^ [[E1]] = int ^ [[A[E1]]] = [[E2]] ^ [[A]] = α[]
```

- Попробуйте протипизировать программу со слайда

```
main() {                    [[main]] = () → ()
var x,y,z,t;
x = {2,4,8,16,32,64};       [[x]] = [[{2,4,8,16,32,64}]] ^ [[2]] = ... = [[64]]         // = int[]
y = x[x[3]];                [[y]] = ([[ x[x[3]] ]] = α_x ^ [[ x[3] ]] = int ^ [[x]] = α_x[]) ^ [[x[3]]] = α_x3 ^ [[3]] = int ^ [[x]] = α_x3[]
z = {{},x};                 [[z]] = [[{{},x}]] ^ [[{}]] = [[x]]
t = z[1];                   [[t]] = ([[ z[1] ]] = α_z ^ [[z]] = α_z[])
t[2] = y;                   [[y]] = α_t ^ [[ t[2] ]] = [[y]] ^ [[t]] = α_t[]
}
```

Решение:

```
[[x]] = int[]      [[a_x]] = int  [[a_x3]] = int
[[y]] = int          
[[z]] = int[][]     a_z = int[]
[[t]] = int[]       a_t = int
```

### Подумайте, что происходит в получившейся реализации, если в программе есть рекурсивный тип?

Используемый солвер основан на union find и позволяет регулярные рекурсивные термы.

# <a name="lec_3"></a> Lecture 3

### У решетки есть максимальный и минимальный элементы (T, ⊥). Являются ли они точной верхней или нижней гранью какого-либо подмножества S?

Да. Точная верхняя грань - минимальная из верхних граней (нижняя мин. соотв.). Например, если взять подмножество из всех
элементов кроме Top и Bottom, то Top и Bottom будут точными гранями.

### Уникальны ли они?

Зависит от решетки. Например нет, если в решетке один элемент.

### Как выглядит T и ⊥ L1 × L2 × . . . × Ln?

T - множество состоящее из T каждой решетки L

⊥ - множество состоящее из ⊥ каждой решетки L

### Какая высота у произведения решеток L1 × L2 × . . . × Ln?

height(L1×. . .×Ln) = height(L1)+...+height(Ln)

### Точная верхняя/нижняя граней решетки отображений

Top - отображение всех элементов в T, для Bottom аналогично в ⊥

### Высота решетки отображений

height(A → L) = |A| · height(L).

### Можно ли выразить анализ типов с предыдущей лекции как анализ над решетками?

Да. В Bottom у нас может быть что то типа Object, а Top - ошибка. Остальные типы - в одной плоскости. Если бы было
наследование, то не в одной - дочерние типы являлись бы верхней гранью родительских.

### Можно ли выразить анализ над решетками как анализ типов?

Да, как будто достаточно перевернуть решетку из предыдущего ответа

# <a name="lec_4"></a> Lecture 4

### Какова сложность структурного алгоритма? А если в CFG нет циклов? Какова сложность по памяти?

Алгоритм обходит все узлы CFG, пусть n - количество узлов. На каждом шаге мы добавляем/удаляем
используемые/неиспользуемые
переменные, сложность такой операции O(k), где k - количество переменных в программе (высота решетки). Также у нас могут
добавляться зависимые узлы в ворклист и увеличивать тем самым количество обходов, но даже в
крайнем случае мы упремся в Top и достигнем фикспоинта для всех переменных за k проходов всей программы (так как это
высота решетки).

Таким образом временная сложность O(n*k*k).

Если циклов нет, то у нас программа выполняется просто сверху нет, а узлы по порядку и так изначально лежат в ворклисте.
То есть, сложность станет O(n*k).

Сложность по памяти - O(n*k). Мы просто храним используемые переменные для каждого узла программы.

### Попробуйте расписать систему ограничений для примера и решить её

```
var x,a,b;          {}
x = input;          {}
a = x-1;            {x-1}
b = x-2;            {x-1, x-2}
while (x > 0) {     {x-1, x-2, x > 0}
output a*b-x;       {a*b-x, a*b, x-1, x-2, x > 0}
x = x-1;            {a*b, x-1}
}
output a*b;         {a*b, x-1}
```

# <a name="lec_5"></a> Lecture 5

### Предложите решетку для реализации анализа размера переменных

#### - Нужно описать не только решетку для одного абстрактного значения, но и все другие решетки, требуемые для анализа целой программы

#### - Опишите правила вычисления различных выражений

#### - Придумайте нетривиальный пример программы на TIP для получившегося анализа и посмотрите, что для него получается

Одна решетка должна состоять из интервалов допустимых значений типов (bool, byte, int, short...) и +-inf (как в
практике) - до этих значений мы будем "расширять" значения переменных (widening). Это позволит узнать размер переменной
в рамках имеющихся типов.

Соответсвенно, чтобы привязать эти значения к переменным, нужна решетка отображений.

[Пример программы](./examples/lec5varsize.tip)

Результат:

```
z -> (98,98)
w -> (-inf,+inf)
u -> (0,1)
y -> (-128,2147483648)
x -> (65535,2147483647)
```

# <a name="lec_6"></a> Lecture 6

### Напишите вариант программы, для которой анализ открытости-закрытости файлов не показывает корректный результат даже с учётом всех возможных условий в переходах

Предложенный анализ не умеет в межпроцедурность. Достаточно заменить условие на вызов функции, и анализ потеряется.

```
main() {
    var flag;
    flag = 1;
    open();
    if (canClose(flag)) close();
    return 0;
}

canClose(v) {
   return v;
}
```

Также достаточно поместить изменение флага и открытие/закрытие файла в разные контексты.

### Предложите, каким образом можно решить описанные в лекции проблемы в этой ситуации

Поддержать межпроцедурный анализ.

# <a name="lec_8"></a> Lecture 8

### Напишите вариант программы, для которой контекстно-чувствительный анализ знаков требует коэффициент k > 1

По сути k это просто глубина вызвов, на которую мы можем уйти. Любая программа, в которой глубина вызовов больше 1
требует соответсвующего k.

```
main() {
    var x1, x2;
    x1 = Xplus5_deep(-10, 3);
    x2 = Xplus5_deep(-10, 2)
    return 0;
}

Xplus5_deep(x, k) {
    if (k == 0) return x;
    else return Xplus5_deep(x+5, k-1);
}
```

### Приведите пример решётки, для которой контекстно-чувствительный анализ в функциональном стиле является более ресурсозатратным, чем контекстно-чувствительный анализ по месту вызова с глубиной 2

Решетка для контекстно чувствительного анализа (string calls): `Nodes → Contexts → lift(States)`
где `Contexts = Calls^(≤k)`

Решетка для функционального подхода:  `Nodes → States → lift(States)`

Из формул видно, что основное различие в решетках - использование Context и States. Соответственно, для того, чтобы
анализ в функциональном стиле был более ресурсозатратным при глубине 2, достаточно выбрать решетку с большим числом
состояний. Например, решетка с переменными или выражениями.