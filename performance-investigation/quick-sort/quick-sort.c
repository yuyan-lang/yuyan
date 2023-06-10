#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef struct Node {
    int data;
    struct Node* next;
} Node;

Node* createNode(int data) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    if (newNode == NULL) {
        printf("Memory allocation failed.\n");
        exit(1);
    }
    newNode->data = data;
    newNode->next = NULL;
    return newNode;
}

Node* insertNode(Node* head, int data) {
    Node* newNode = createNode(data);
    if (head == NULL) {
        return newNode;
    }
    Node* current = head;
    while (current->next != NULL) {
        current = current->next;
    }
    current->next = newNode;
    return head;
}

void printList(Node* head) {
    Node* current = head;
    while (current != NULL) {
        printf("%d ", current->data);
        current = current->next;
    }
    printf("\n");
}

Node* getTail(Node* head) {
    Node* current = head;
    while (current != NULL && current->next != NULL) {
        current = current->next;
    }
    return current;
}

Node* partition(Node* head, Node* end, Node** newHead, Node** newEnd) {
    Node* pivot = end;
    Node* prev = NULL;
    Node* current = head;
    Node* tail = pivot;

    while (current != pivot) {
        if (current->data < pivot->data) {
            if (*newHead == NULL) {
                *newHead = current;
            }
            prev = current;
            current = current->next;
        } else {
            if (prev) {
                prev->next = current->next;
            }
            Node* temp = current->next;
            current->next = NULL;
            tail->next = current;
            tail = current;
            current = temp;
        }
    }

    if (*newHead == NULL) {
        *newHead = pivot;
    }

    *newEnd = tail;

    return pivot;
}

Node* quicksortRec(Node* head, Node* end) {
    if (head == NULL || head == end) {
        return head;
    }

    Node* newHead = NULL;
    Node* newEnd = NULL;
    Node* pivot = partition(head, end, &newHead, &newEnd);

    if (newHead != pivot) {
        Node* temp = newHead;
        while (temp->next != pivot) {
            temp = temp->next;
        }
        temp->next = NULL;
        newHead = quicksortRec(newHead, temp);
        temp = getTail(newHead);
        temp->next = pivot;
    }

    pivot->next = quicksortRec(pivot->next, newEnd);

    return newHead;
}

void quicksort(Node** headRef) {
    *headRef = quicksortRec(*headRef, getTail(*headRef));
}

void freeList(Node* head) {
    Node* current = head;
    while (current != NULL) {
        Node* temp = current;
        current = current->next;
        free(temp);
    }
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: ./quicksort <length>\n");
        return 1;
    }

    int n = atoi(argv[1]);

    srand(time(NULL));
    Node* head = NULL;

    for (int i = 0; i < n; i++) {
        int value = rand() % 10000001;  // Generate random value between 0 and 10,000,000
        head = insertNode(head, value);
    }

    // printf("Original list: ");
    // printList(head);
    clock_t start = clock();
    quicksort(&head);
    clock_t end = clock();

    double time_taken = (double)(end - start) / CLOCKS_PER_SEC;

    // printf("Sorted list: ");
    // printList(head);
    printf("%.6f\n", time_taken);

    freeList(head);

    return 0;
}