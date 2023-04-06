# l = [0,1,2,3,4]
# print(l[:0])
# print(l[:1])

# print (f'does this work: %s' % 42)

# for i in range(10):
#     print((i+1)%2)

def reorder_list(lst, idx):
    # Split the list into two parts: from index to end, and from start to index
    lst1 = lst[idx:]
    lst2 = lst[:idx]
    # Concatenate the two parts in the desired order
    reordered_list = lst1 + lst2

    return reordered_list

lst = [1, 2, 3, 4, 5]
idx = 2
reordered_lst = reorder_list(lst, idx)
print(reordered_lst)

