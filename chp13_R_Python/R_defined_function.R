Fold = function(Z, w, D, seed = 7777) {
    n = nrow(w); d = 1:n; dd = list()
    w[,D] = factor(w[,D])
    e = levels(w[,D]); T = length(e) # 因变量T类
    set.seed(seed)
    for (i in 1:T) {
        d0 = d[w[, D] == e[i]]; j = length(d0)
        ZT = rep(1:Z, ceiling(j/Z))[1:j]
        id = cbind(sample(ZT, length(ZT), d0)); dd[[i]] = id
    } # 上面每个dd[[i]]是一个矩阵，第一列是分组，第二列是样本
    mm = list()
    for (i in 1:Z){
        u = Null;
        for(j in 1:T) u = c(u, dd[[j]][dd[[j]][,1] == i, 2])
        mm[[i]] = u
    } # 上面每个mm[[i]]是一个向量，表示第i组的样本
    return(mm)
}