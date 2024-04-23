def normsb(a, ub, lb):
    return (a - lb) / (ub - lb)

def normax(a):
    return a / max(a)

class Attribute:
    def __init__(self, a, ub, lb):

        if(ub < lb):
            raise Exception("Upper bound 'ub' must be greater or equal to the lower bound 'lb'.")

        self.a = a
        self.ub = ub
        self.lb = lb
        # self.normsb = normsb(
        #     a = self.a,
        #     ub = self.ub,
        #     lb = self.lb
        # )
        # self.normax = normax(self.a)

dsds = Attribute(100, 100, 0)
dsds.a
dsds.lb
dsds.ub
dsds.normsb