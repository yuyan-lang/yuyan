; ModuleID = 'matrix-multiply.0.opt.bc'
source_filename = "matrix-multiply.0.ll"

@CommandLine_argc = external hidden local_unnamed_addr global i32
@staticHeapI = external hidden constant i8
@staticHeapM = external hidden global i8
@CommandLine_argv = external hidden local_unnamed_addr global i64
@nextChunks = external hidden local_unnamed_addr constant [84 x i64 (i8*, i8*, i8*, i64)*]

define hidden i64 @Chunk_0(i8* %gcState, i8* %stackTopArg, i8* %frontierArg, i64 %nextBlockArg) local_unnamed_addr {
start:
  %t87 = getelementptr inbounds i8, i8* %stackTopArg, i64 -24
  %t88 = bitcast i8* %t87 to i8**
  %t91 = bitcast i8* %stackTopArg to i8**
  %t92 = load i8*, i8** %t91, align 8
  store i8* %t92, i8** %t88, align 8
  %t93 = getelementptr inbounds i8, i8* %gcState, i64 8
  %t94 = bitcast i8* %t93 to i8**
  %t95 = load i8*, i8** %t94, align 8
  %t97.not = icmp ult i8* %t95, %frontierArg
  br i1 %t97.not, label %L_45, label %start.L_47_crit_edge

start.L_47_crit_edge:                             ; preds = %start
  %t85 = getelementptr inbounds i8, i8* %stackTopArg, i64 -32
  br label %L_47

L_45:                                             ; preds = %start
  %t4 = getelementptr inbounds i8, i8* %stackTopArg, i64 8
  %t5 = bitcast i8* %t4 to i64*
  store i64 156, i64* %t5, align 4
  %t7 = getelementptr inbounds i8, i8* %stackTopArg, i64 16
  %t9 = bitcast i8* %gcState to i8**
  store i8* %frontierArg, i8** %t9, align 8
  %t11 = getelementptr inbounds i8, i8* %gcState, i64 16
  %t12 = bitcast i8* %t11 to i8**
  store i8* %t7, i8** %t12, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t17 = load i8*, i8** %t9, align 8
  %t20 = load i8*, i8** %t12, align 8
  %t2 = getelementptr inbounds i8, i8* %t20, i64 -48
  br label %L_47

L_47:                                             ; preds = %start.L_47_crit_edge, %L_45
  %frontier.0 = phi i8* [ %t17, %L_45 ], [ %frontierArg, %start.L_47_crit_edge ]
  %stackTop.0 = phi i8* [ %t2, %L_45 ], [ %t85, %start.L_47_crit_edge ]
  %t79.pre-phi = bitcast i8* %gcState to i8**
  %0 = bitcast i8* %stackTop.0 to i8***
  %t241 = load i8**, i8*** %0, align 8
  %t27 = load i8*, i8** %t241, align 8
  %t29 = getelementptr inbounds i8, i8* %frontier.0, i64 8
  %t34 = bitcast i8* %frontier.0 to i64*
  store i64 127, i64* %t34, align 4
  %t36 = getelementptr inbounds i8, i8* %frontier.0, i64 32
  %t39 = bitcast i8* %t29 to i8**
  %t41 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  %t42 = bitcast i8* %t41 to i8**
  %t43 = load i8*, i8** %t42, align 8
  store i8* %t43, i8** %t39, align 8
  %t45 = getelementptr inbounds i8, i8* %frontier.0, i64 16
  %t46 = bitcast i8* %t45 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 3592), i8** %t46, align 8
  %t50 = getelementptr inbounds i8, i8* %frontier.0, i64 24
  %t51 = bitcast i8* %t50 to i8**
  store i8* %t27, i8** %t51, align 8
  %t53 = getelementptr inbounds i8, i8* %gcState, i64 32
  %t54 = bitcast i8* %t53 to i64*
  %t56 = getelementptr inbounds i8, i8* %stackTop.0, i64 16
  %t57 = bitcast i8* %t56 to i64*
  %t58 = load i64, i64* %t57, align 4
  store i64 %t58, i64* %t54, align 4
  %t59 = getelementptr inbounds i8, i8* %gcState, i64 1560
  %t60 = bitcast i8* %t59 to i8**
  %t61 = load i8*, i8** %t60, align 8
  %t65 = getelementptr inbounds i8, i8* %t61, i64 %t58
  %t68 = bitcast i8* %t65 to i8**
  store i8* %t29, i8** %t68, align 8
  %t73 = load i8*, i8** %t60, align 8
  %t76 = load i64, i64* %t54, align 4
  %t77 = getelementptr inbounds i8, i8* %t73, i64 %t76
  store i8* %t36, i8** %t79.pre-phi, align 8
  %t81 = getelementptr inbounds i8, i8* %gcState, i64 16
  %t82 = bitcast i8* %t81 to i8**
  store i8* %t77, i8** %t82, align 8
  ret i64 61
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn
define hidden i64 @Chunk_2(i8* nocapture %gcState, i8* nocapture readonly %stackTopArg, i8* %frontierArg, i64 %nextBlockArg) local_unnamed_addr #0 {
start:
  %t3 = getelementptr inbounds i8, i8* %gcState, i64 32
  %t4 = bitcast i8* %t3 to i64*
  %t6 = getelementptr inbounds i8, i8* %stackTopArg, i64 -16
  %t7 = bitcast i8* %t6 to i64*
  %t8 = load i64, i64* %t7, align 4
  store i64 %t8, i64* %t4, align 4
  %t9 = getelementptr inbounds i8, i8* %gcState, i64 1560
  %t10 = bitcast i8* %t9 to i8**
  %t11 = load i8*, i8** %t10, align 8
  %t15 = getelementptr inbounds i8, i8* %t11, i64 %t8
  %t17 = getelementptr inbounds i8, i8* %t15, i64 -8
  %t18 = bitcast i8* %t17 to i64*
  %t19 = load i64, i64* %t18, align 4
  %t23 = bitcast i8* %gcState to i8**
  store i8* %frontierArg, i8** %t23, align 8
  %t25 = getelementptr inbounds i8, i8* %gcState, i64 16
  %t26 = bitcast i8* %t25 to i8**
  store i8* %t15, i8** %t26, align 8
  ret i64 %t19
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn
define hidden i64 @Chunk_3(i8* nocapture %gcState, i8* nocapture readonly %stackTopArg, i8* %frontierArg, i64 %nextBlockArg) local_unnamed_addr #0 {
start:
  %t3 = getelementptr inbounds i8, i8* %gcState, i64 32
  %t4 = bitcast i8* %t3 to i64*
  %t6 = getelementptr inbounds i8, i8* %stackTopArg, i64 -16
  %t7 = bitcast i8* %t6 to i64*
  %t8 = load i64, i64* %t7, align 4
  store i64 %t8, i64* %t4, align 4
  %t9 = getelementptr inbounds i8, i8* %gcState, i64 1560
  %t10 = bitcast i8* %t9 to i8**
  %t11 = load i8*, i8** %t10, align 8
  %t15 = getelementptr inbounds i8, i8* %t11, i64 %t8
  %t17 = bitcast i8* %gcState to i8**
  store i8* %frontierArg, i8** %t17, align 8
  %t19 = getelementptr inbounds i8, i8* %gcState, i64 16
  %t20 = bitcast i8* %t19 to i8**
  store i8* %t15, i8** %t20, align 8
  ret i64 9
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn
define hidden i64 @Chunk_5(i8* nocapture %gcState, i8* nocapture readonly %stackTopArg, i8* %frontierArg, i64 %nextBlockArg) local_unnamed_addr #0 {
start:
  %t3 = getelementptr inbounds i8, i8* %gcState, i64 32
  %t4 = bitcast i8* %t3 to i64*
  %t6 = getelementptr inbounds i8, i8* %stackTopArg, i64 -16
  %t7 = bitcast i8* %t6 to i64*
  %t8 = load i64, i64* %t7, align 4
  store i64 %t8, i64* %t4, align 4
  %t9 = getelementptr inbounds i8, i8* %gcState, i64 1560
  %t10 = bitcast i8* %t9 to i8**
  %t11 = load i8*, i8** %t10, align 8
  %t15 = getelementptr inbounds i8, i8* %t11, i64 %t8
  %t17 = bitcast i8* %gcState to i8**
  store i8* %frontierArg, i8** %t17, align 8
  %t19 = getelementptr inbounds i8, i8* %gcState, i64 16
  %t20 = bitcast i8* %t19 to i8**
  store i8* %t15, i8** %t20, align 8
  ret i64 9
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn
define hidden i64 @Chunk_6(i8* nocapture %gcState, i8* nocapture readonly %stackTopArg, i8* %frontierArg, i64 %nextBlockArg) local_unnamed_addr #0 {
start:
  %t3 = getelementptr inbounds i8, i8* %gcState, i64 32
  %t4 = bitcast i8* %t3 to i64*
  %t6 = getelementptr inbounds i8, i8* %stackTopArg, i64 -16
  %t7 = bitcast i8* %t6 to i64*
  %t8 = load i64, i64* %t7, align 4
  store i64 %t8, i64* %t4, align 4
  %t9 = getelementptr inbounds i8, i8* %gcState, i64 1560
  %t10 = bitcast i8* %t9 to i8**
  %t11 = load i8*, i8** %t10, align 8
  %t15 = getelementptr inbounds i8, i8* %t11, i64 %t8
  %t17 = bitcast i8* %gcState to i8**
  store i8* %frontierArg, i8** %t17, align 8
  %t19 = getelementptr inbounds i8, i8* %gcState, i64 16
  %t20 = bitcast i8* %t19 to i8**
  store i8* %t15, i8** %t20, align 8
  ret i64 9
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn
define hidden i64 @Chunk_7(i8* nocapture %gcState, i8* nocapture readonly %stackTopArg, i8* %frontierArg, i64 %nextBlockArg) local_unnamed_addr #0 {
start:
  %t3 = getelementptr inbounds i8, i8* %gcState, i64 32
  %t4 = bitcast i8* %t3 to i64*
  %t6 = getelementptr inbounds i8, i8* %stackTopArg, i64 -16
  %t7 = bitcast i8* %t6 to i64*
  %t8 = load i64, i64* %t7, align 4
  store i64 %t8, i64* %t4, align 4
  %t9 = getelementptr inbounds i8, i8* %gcState, i64 1560
  %t10 = bitcast i8* %t9 to i8**
  %t11 = load i8*, i8** %t10, align 8
  %t15 = getelementptr inbounds i8, i8* %t11, i64 %t8
  %t17 = bitcast i8* %gcState to i8**
  store i8* %frontierArg, i8** %t17, align 8
  %t19 = getelementptr inbounds i8, i8* %gcState, i64 16
  %t20 = bitcast i8* %t19 to i8**
  store i8* %t15, i8** %t20, align 8
  ret i64 9
}

define hidden i64 @Chunk_8(i8* %gcState, i8* %stackTopArg, i8* %frontierArg, i64 %nextBlockArg) local_unnamed_addr {
start:
  tail call void @Stdio_print(i8* getelementptr (i8, i8* @staticHeapI, i64 3664))
  %t8 = getelementptr inbounds i8, i8* %stackTopArg, i64 8
  %t9 = bitcast i8* %t8 to i64*
  store i64 94, i64* %t9, align 4
  %t11 = getelementptr inbounds i8, i8* %stackTopArg, i64 16
  %t13 = bitcast i8* %gcState to i8**
  store i8* %frontierArg, i8** %t13, align 8
  %t15 = getelementptr inbounds i8, i8* %gcState, i64 16
  %t16 = bitcast i8* %t15 to i8**
  store i8* %t11, i8** %t16, align 8
  tail call void @MLton_halt(i8* %gcState, i32 1)
  tail call void @MLton_bug(i8* getelementptr (i8, i8* @staticHeapI, i64 3624))
  %t6 = tail call i64 @MLton_unreachable()
  ret i64 %t6
}

define hidden i64 @Chunk_9(i8* nocapture readnone %gcState, i8* nocapture readnone %stackTopArg, i8* nocapture readnone %frontierArg, i64 %nextBlockArg) local_unnamed_addr {
start:
  tail call void @MLton_bug(i8* getelementptr (i8, i8* @staticHeapI, i64 3728))
  %t6 = tail call i64 @MLton_unreachable()
  ret i64 %t6
}

define hidden i64 @Chunk_4(i8* %gcState, i8* %stackTopArg, i8* %frontierArg, i64 %nextBlockArg) local_unnamed_addr {
start:
  tail call void @Stdio_print(i8* getelementptr (i8, i8* @staticHeapI, i64 3840))
  %t10 = getelementptr inbounds i8, i8* %stackTopArg, i64 8
  %t11 = bitcast i8* %t10 to i64*
  store i64 94, i64* %t11, align 4
  %t13 = getelementptr inbounds i8, i8* %stackTopArg, i64 16
  %t15 = bitcast i8* %gcState to i8**
  store i8* %frontierArg, i8** %t15, align 8
  %t17 = getelementptr inbounds i8, i8* %gcState, i64 16
  %t18 = bitcast i8* %t17 to i8**
  store i8* %t13, i8** %t18, align 8
  tail call void @MLton_halt(i8* %gcState, i32 1)
  tail call void @MLton_bug(i8* getelementptr (i8, i8* @staticHeapI, i64 3624))
  %t8 = tail call i64 @MLton_unreachable()
  ret i64 %t8
}

define hidden i64 @Chunk_10(i8* nocapture readnone %gcState, i8* nocapture readnone %stackTopArg, i8* nocapture readnone %frontierArg, i64 %nextBlockArg) local_unnamed_addr {
start:
  tail call void @MLton_bug(i8* getelementptr (i8, i8* @staticHeapI, i64 3728))
  %t6 = tail call i64 @MLton_unreachable()
  ret i64 %t6
}

define hidden i64 @Chunk_11(i8* %gcState, i8* %stackTopArg, i8* %frontierArg, i64 %nextBlockArg) {
start:
  %t5410 = getelementptr inbounds i8, i8* %gcState, i64 8
  %t5411 = bitcast i8* %t5410 to i8**
  %t5312 = bitcast i8* %gcState to i8**
  %t5314 = getelementptr inbounds i8, i8* %gcState, i64 16
  %t5315 = bitcast i8* %t5314 to i8**
  %t4074 = getelementptr inbounds i8, i8* %gcState, i64 1560
  %t4075 = bitcast i8* %t4074 to i8**
  %t4080 = getelementptr inbounds i8, i8* %gcState, i64 32
  %t4081 = bitcast i8* %t4080 to i64*
  %t4311 = getelementptr inbounds i8, i8* %gcState, i64 24
  %t4312 = bitcast i8* %t4311 to i8**
  %t2078 = getelementptr inbounds i8, i8* %gcState, i64 68
  %t2079 = bitcast i8* %t2078 to i32*
  br label %doSwitchNextBlock

doSwitchNextBlock:                                ; preds = %doSwitchNextBlock.backedge, %start
  %stackTop.0 = phi i8* [ %stackTopArg, %start ], [ %stackTop.0.be, %doSwitchNextBlock.backedge ]
  %frontier.0 = phi i8* [ %frontierArg, %start ], [ %frontier.0.be, %doSwitchNextBlock.backedge ]
  %nextBlock.0 = phi i64 [ %nextBlockArg, %start ], [ %nextBlock.0.be, %doSwitchNextBlock.backedge ]
  switch i64 %nextBlock.0, label %switchNextBlockDefault [
    i64 11, label %L_1828
    i64 12, label %put_0
    i64 13, label %L_1829
    i64 14, label %L_35
    i64 15, label %x_7
    i64 16, label %put_0
    i64 17, label %L_345
    i64 18, label %x_1
    i64 19, label %L_1821
    i64 20, label %L_447
    i64 21, label %L_1826
    i64 22, label %L_440
    i64 23, label %L_1825
    i64 24, label %L_1823
    i64 25, label %L_511
    i64 26, label %exnMessage_0
    i64 27, label %L_1819
    i64 28, label %L_1818
    i64 29, label %L_1817
    i64 30, label %L_1816
    i64 31, label %L_557
    i64 32, label %L_268
    i64 33, label %L_1261
    i64 34, label %L_1261
    i64 35, label %L_1766
    i64 36, label %L_1765
    i64 37, label %print_5
    i64 38, label %L_1718
  ]

switchNextBlockDefault:                           ; preds = %doSwitchNextBlock
  unreachable

L_6:                                              ; preds = %L_1828, %L_5
  %t4 = getelementptr inbounds i8, i8* %stackTop.0, i64 40
  %t5 = bitcast i8* %t4 to i64*
  store i64 156, i64* %t5, align 4
  %t7 = getelementptr inbounds i8, i8* %stackTop.0, i64 48
  store i8* %frontier.0, i8** %t5312, align 8
  store i8* %t7, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t17 = load i8*, i8** %t5312, align 8
  %t20 = load i8*, i8** %t5315, align 8
  %t2 = getelementptr inbounds i8, i8* %t20, i64 -48
  br label %L_8

L_80.L_80_crit_edge:                              ; preds = %L_80.preheader, %L_80.L_80_crit_edge
  %t421409 = phi i64 [ %t42, %L_80.L_80_crit_edge ], [ 1, %L_80.preheader ]
  %t27.pre = load i8*, i8** %t212, align 8
  %t23 = add i64 %t421409, %t107
  %t30 = getelementptr inbounds i8, i8* %t27.pre, i64 %t421409
  %t33 = load i8, i8* %t30, align 1
  %t37 = getelementptr inbounds i8, i8* %t187, i64 %t23
  store i8 %t33, i8* %t37, align 1
  %t42 = add i64 %t421409, 1
  %t46.not = icmp slt i64 %t50, %t42
  br i1 %t46.not, label %L_75, label %L_80.L_80_crit_edge

L_79:                                             ; preds = %L_72
  %t50 = add i64 %t216, -1
  %t46.not906 = icmp slt i64 %t50, 0
  br i1 %t46.not906, label %L_75, label %L_80.preheader

L_80.preheader:                                   ; preds = %L_79
  %t331406 = load i8, i8* %t213, align 1
  %t371407 = getelementptr inbounds i8, i8* %t187, i64 %t107
  store i8 %t331406, i8* %t371407, align 1
  %t46.not1408 = icmp eq i64 %t50, 0
  br i1 %t46.not1408, label %L_75, label %L_80.L_80_crit_edge

L_76:                                             ; preds = %L_75
  %t54 = getelementptr inbounds i8, i8* %stackTop.13, i64 64
  %t55 = bitcast i8* %t54 to i64*
  store i64 176, i64* %t55, align 4
  %t57 = getelementptr inbounds i8, i8* %stackTop.13, i64 72
  store i8* %t196, i8** %t5312, align 8
  store i8* %t57, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t67 = load i8*, i8** %t5312, align 8
  %t70 = load i8*, i8** %t5315, align 8
  %t52 = getelementptr inbounds i8, i8* %t70, i64 -72
  br label %L_78

L_78:                                             ; preds = %L_75, %L_76
  %stackTop.1 = phi i8* [ %t52, %L_76 ], [ %stackTop.13, %L_75 ]
  %frontier.1 = phi i8* [ %t67, %L_76 ], [ %t196, %L_75 ]
  %t76 = getelementptr inbounds i8, i8* %stackTop.1, i64 56
  br label %flushOut_0.sink.split

L_75:                                             ; preds = %L_80.L_80_crit_edge, %L_80.preheader, %L_79, %L_73
  %t81 = load i8*, i8** %t5411, align 8
  %t83.not = icmp ult i8* %t81, %t196
  br i1 %t83.not, label %L_76, label %L_78

L_73:                                             ; preds = %L_72
  tail call void @GC_sequenceCopy(i8* nonnull %gcState, i8* nonnull %t187, i64 %t107, i8* nonnull %t213, i64 0, i64 %t216)
  br label %L_75

L_72:                                             ; preds = %L_70
  %t95 = icmp sgt i64 %t216, 4
  br i1 %t95, label %L_73, label %L_79

L_70:                                             ; preds = %L_68
  %t107 = sext i32 %t209 to i64
  %t110.not = icmp ult i64 %t156, %t107
  %t100 = add i64 %t216, %t107
  %t103.not = icmp ult i64 %t156, %t100
  %or.cond = select i1 %t110.not, i1 true, i1 %t103.not
  br i1 %or.cond, label %L_11, label %L_72

L_69:                                             ; preds = %L_68
  %t116 = getelementptr inbounds i8, i8* %stackTop.13, i64 24
  %t117 = bitcast i8* %t116 to i64*
  store i64 14, i64* %t117, align 4
  %t119 = getelementptr inbounds i8, i8* %stackTop.13, i64 32
  %t123 = load i8*, i8** %t4075, align 8
  %t124 = ptrtoint i8* %t119 to i64
  %t125 = ptrtoint i8* %t123 to i64
  %t126 = sub i64 %t124, %t125
  store i64 %t126, i64* %t4081, align 4
  %t130 = getelementptr inbounds i8, i8* %stackTop.13, i64 64
  %t131 = bitcast i8* %t130 to i8**
  store i8* %t189, i8** %t131, align 8
  %t134 = getelementptr inbounds i8, i8* %stackTop.13, i64 72
  %t135 = bitcast i8* %t134 to i8**
  %t139 = load i8*, i8** %t1120, align 8
  store i8* %t139, i8** %t135, align 8
  %t142 = bitcast i8* %t170 to i64*
  store i64 12, i64* %t142, align 4
  br label %L_345

L_68:                                             ; preds = %L_67
  %t150.not = icmp slt i32 %t168, %t158
  br i1 %t150.not, label %L_70, label %L_69

L_67:                                             ; preds = %L_66
  %t154 = getelementptr inbounds i8, i8* %t187, i64 -16
  %t155 = bitcast i8* %t154 to i64*
  %t156 = load i64, i64* %t155, align 4
  %t158 = trunc i64 %t156 to i32
  %t160 = sext i32 %t158 to i64
  %t163.not = icmp eq i64 %t156, %t160
  br i1 %t163.not, label %L_68, label %L_11

L_66:                                             ; preds = %L_65
  %t168 = add i32 %t209, %t218
  %t170 = getelementptr inbounds i8, i8* %stackTop.13, i64 56
  %t171 = bitcast i8* %t170 to i32*
  store i32 %t168, i32* %t171, align 4
  %t174 = tail call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %t218, i32 %t209)
  %t175 = extractvalue { i32, i1 } %t174, 1
  br i1 %t175, label %L_11, label %L_67

L_65:                                             ; preds = %L_8
  %t180 = getelementptr inbounds i8, i8* %t1131, i64 8
  %t181 = bitcast i8* %t180 to i8**
  %t182 = load i8*, i8** %t181, align 8
  %t186 = bitcast i8* %t1131 to i8**
  %t187 = load i8*, i8** %t186, align 8
  %t189 = getelementptr inbounds i8, i8* %frontier.13, i64 8
  %t194 = bitcast i8* %frontier.13 to i64*
  store i64 79, i64* %t194, align 4
  %t196 = getelementptr inbounds i8, i8* %frontier.13, i64 24
  %t199 = bitcast i8* %t189 to i8**
  store i8* %t182, i8** %t199, align 8
  %t202 = getelementptr inbounds i8, i8* %frontier.13, i64 16
  %t203 = bitcast i8* %t202 to i8**
  store i8* %t187, i8** %t203, align 8
  %t209 = load i32, i32* bitcast (i8* getelementptr (i8, i8* @staticHeapM, i64 8) to i32*), align 4
  %t212 = bitcast i8* %stackTop.13 to i8**
  %t213 = load i8*, i8** %t212, align 8
  %t214 = getelementptr inbounds i8, i8* %t213, i64 -16
  %t215 = bitcast i8* %t214 to i64*
  %t216 = load i64, i64* %t215, align 4
  %t218 = trunc i64 %t216 to i32
  %t220 = sext i32 %t218 to i64
  %t223.not = icmp eq i64 %t216, %t220
  br i1 %t223.not, label %L_66, label %L_11

L_52:                                             ; preds = %L_1830
  %t229 = getelementptr inbounds i8, i8* %stackTop.13, i64 96
  %t230 = bitcast i8* %t229 to i64*
  store i64 175, i64* %t230, align 4
  %t232 = getelementptr inbounds i8, i8* %stackTop.13, i64 104
  store i8* %t1054, i8** %t5312, align 8
  store i8* %t232, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t242 = load i8*, i8** %t5312, align 8
  %t245 = load i8*, i8** %t5315, align 8
  %t227 = getelementptr inbounds i8, i8* %t245, i64 -104
  %t354.phi.trans.insert = getelementptr inbounds i8, i8* %t245, i64 -40
  %t355.phi.trans.insert = bitcast i8* %t354.phi.trans.insert to i32*
  %t356.pre = load i32, i32* %t355.phi.trans.insert, align 4
  %t359.phi.trans.insert = getelementptr inbounds i8, i8* %t245, i64 -24
  %t360.phi.trans.insert = bitcast i8* %t359.phi.trans.insert to i64*
  %t361.pre = load i64, i64* %t360.phi.trans.insert, align 4
  br label %L_54

L_64:                                             ; preds = %L_64.lr.ph, %L_64
  %TW64_1.0912 = phi i64 [ 0, %L_64.lr.ph ], [ %t267, %L_64 ]
  %t248 = add i64 %TW64_1.0912, %t357
  %t252 = load i8*, i8** %t251, align 8
  %t255 = getelementptr inbounds i8, i8* %t252, i64 %TW64_1.0912
  %t258 = load i8, i8* %t255, align 1
  %t262 = getelementptr inbounds i8, i8* %t321, i64 %t248
  store i8 %t258, i8* %t262, align 1
  %t267 = add i64 %TW64_1.0912, 1
  %t272 = load i64, i64* %t271, align 4
  %t274.not = icmp slt i64 %t272, %t267
  br i1 %t274.not, label %L_59, label %L_64

L_63:                                             ; preds = %L_56
  %t270 = getelementptr inbounds i8, i8* %stackTop.3, i64 88
  %t271 = bitcast i8* %t270 to i64*
  %t272910 = load i64, i64* %t271, align 4
  %t274.not911 = icmp slt i64 %t272910, 0
  br i1 %t274.not911, label %L_59, label %L_64.lr.ph

L_64.lr.ph:                                       ; preds = %L_63
  %t251 = bitcast i8* %stackTop.3 to i8**
  br label %L_64

L_60:                                             ; preds = %L_59
  store i64 174, i64* %t342, align 4
  store i8* %frontier.3, i8** %t5312, align 8
  store i8* %t359, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t297 = load i8*, i8** %t5312, align 8
  %t300 = load i8*, i8** %t5315, align 8
  %t282 = getelementptr inbounds i8, i8* %t300, i64 -80
  br label %L_62

L_62:                                             ; preds = %L_59, %L_60
  %stackTop.2 = phi i8* [ %t282, %L_60 ], [ %stackTop.3, %L_59 ]
  %frontier.2 = phi i8* [ %t297, %L_60 ], [ %frontier.3, %L_59 ]
  %t302 = getelementptr inbounds i8, i8* %stackTop.2, i64 56
  %0 = bitcast i8* %t302 to i32**
  %t304551 = load i32*, i32** %0, align 8
  %t308 = getelementptr inbounds i8, i8* %stackTop.2, i64 68
  br label %flushOut_0.sink.split

L_59:                                             ; preds = %L_64, %L_63, %L_57
  %t313 = load i8*, i8** %t5411, align 8
  %t315.not = icmp ult i8* %t313, %frontier.3
  br i1 %t315.not, label %L_60, label %L_62

L_57:                                             ; preds = %L_56
  %t325 = bitcast i8* %stackTop.3 to i8**
  %t326 = load i8*, i8** %t325, align 8
  tail call void @GC_sequenceCopy(i8* nonnull %gcState, i8* %t321, i64 %t357, i8* %t326, i64 0, i64 %t343)
  br label %L_59

L_56:                                             ; preds = %L_55
  %t336 = icmp sgt i64 %t343, 4
  %t319 = getelementptr inbounds i8, i8* %stackTop.3, i64 48
  %t320 = bitcast i8* %t319 to i8**
  %t321 = load i8*, i8** %t320, align 8
  br i1 %t336, label %L_57, label %L_63

L_55:                                             ; preds = %L_54
  %t341 = getelementptr inbounds i8, i8* %stackTop.3, i64 72
  %t342 = bitcast i8* %t341 to i64*
  %t343 = load i64, i64* %t342, align 4
  %t344 = add i64 %t343, %t357
  %t350.not = icmp ult i64 %t361, %t344
  br i1 %t350.not, label %L_11, label %L_56

L_54:                                             ; preds = %L_1830, %L_52
  %t361 = phi i64 [ %t361.pre, %L_52 ], [ %t936, %L_1830 ]
  %t356 = phi i32 [ %t356.pre, %L_52 ], [ %t1075, %L_1830 ]
  %stackTop.3 = phi i8* [ %t227, %L_52 ], [ %stackTop.13, %L_1830 ]
  %frontier.3 = phi i8* [ %t242, %L_52 ], [ %t1054, %L_1830 ]
  %t357 = sext i32 %t356 to i64
  %t359 = getelementptr inbounds i8, i8* %stackTop.3, i64 80
  %t363.not = icmp ult i64 %t361, %t357
  br i1 %t363.not, label %L_11, label %L_55

L_1830:                                           ; preds = %loop_1, %L_48
  store i8* %t1045, i8** %t1120, align 8
  %t372 = load i8*, i8** %t5411, align 8
  %t374.not = icmp ult i8* %t372, %t1054
  br i1 %t374.not, label %L_52, label %L_54

L_49:                                             ; preds = %L_48, %loop_1
  %TW64_0.1909 = phi i64 [ %t378, %loop_1 ], [ 0, %L_48 ]
  %t386 = getelementptr inbounds i8, i8* %t1082, i64 %TW64_0.1909
  %t389 = load i8, i8* %t386, align 1
  %t391.not = icmp eq i8 %t389, 10
  %t378 = add i64 %TW64_0.1909, 1
  br i1 %t391.not, label %L_15, label %loop_1

loop_1:                                           ; preds = %L_49
  %t399.not = icmp slt i64 %t406, %t378
  br i1 %t399.not, label %L_1830, label %L_49

L_48:                                             ; preds = %L_14
  %t406 = add i64 %t1085, -1
  %t408 = getelementptr inbounds i8, i8* %stackTop.13, i64 88
  %t409 = bitcast i8* %t408 to i64*
  store i64 %t406, i64* %t409, align 4
  %t399.not908 = icmp slt i64 %t406, 0
  br i1 %t399.not908, label %L_1830, label %L_49

L_16:                                             ; preds = %put_0
  %t414 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t415 = bitcast i8* %t414 to i64*
  store i64 173, i64* %t415, align 4
  store i8* %frontier.0, i8** %t5312, align 8
  store i8* %stackTop.0, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t427 = load i8*, i8** %t5312, align 8
  %t430 = load i8*, i8** %t5315, align 8
  br label %L_18

L_1829:                                           ; preds = %doSwitchNextBlock
  %t432 = getelementptr inbounds i8, i8* %stackTop.0, i64 -48
  %t436 = getelementptr inbounds i8, i8* %stackTop.0, i64 -32
  %t437 = bitcast i8* %t436 to i64*
  %t438 = load i64, i64* %t437, align 4
  store i64 %t438, i64* %t4081, align 4
  %t440 = getelementptr inbounds i8, i8* %stackTop.0, i64 -56
  br label %doSwitchNextBlock.backedge.sink.split

doSwitchNextBlock.backedge.sink.split:            ; preds = %L_545, %L_546, %L_547, %L_548, %L_549, %L_550, %L_551, %L_552, %L_553, %L_554, %L_1816, %L_1817, %L_1819, %L_367, %L_1829
  %t440.sink = phi i8* [ %t440, %L_1829 ], [ %t1180, %L_367 ], [ %t3542, %L_1819 ], [ %t3809, %L_1817 ], [ %t3951, %L_1816 ], [ %t4166, %L_554 ], [ %t4175, %L_553 ], [ %t4184, %L_552 ], [ %t4193, %L_551 ], [ %t4202, %L_550 ], [ %t4211, %L_549 ], [ %t4220, %L_548 ], [ %t4229, %L_547 ], [ %t4238, %L_546 ], [ %t4247, %L_545 ]
  %stackTop.0.be.ph = phi i8* [ %t432, %L_1829 ], [ %stackTop.16, %L_367 ], [ %t3526, %L_1819 ], [ %t3793, %L_1817 ], [ %t3935, %L_1816 ], [ %stackTop.41, %L_554 ], [ %stackTop.41, %L_553 ], [ %stackTop.41, %L_552 ], [ %stackTop.41, %L_551 ], [ %stackTop.41, %L_550 ], [ %stackTop.41, %L_549 ], [ %stackTop.41, %L_548 ], [ %stackTop.41, %L_547 ], [ %stackTop.41, %L_546 ], [ %stackTop.41, %L_545 ]
  %frontier.0.be.ph = phi i8* [ %frontier.0, %L_1829 ], [ %frontier.17, %L_367 ], [ %frontier.0, %L_1819 ], [ %frontier.0, %L_1817 ], [ %frontier.0, %L_1816 ], [ %frontier.41, %L_554 ], [ %frontier.41, %L_553 ], [ %frontier.41, %L_552 ], [ %frontier.41, %L_551 ], [ %frontier.41, %L_550 ], [ %frontier.41, %L_549 ], [ %frontier.41, %L_548 ], [ %frontier.41, %L_547 ], [ %frontier.41, %L_546 ], [ %frontier.41, %L_545 ]
  %t441 = bitcast i8* %t440.sink to i64*
  %t442 = load i64, i64* %t441, align 4
  br label %doSwitchNextBlock.backedge

doSwitchNextBlock.backedge:                       ; preds = %doSwitchNextBlock.backedge.sink.split, %L_366, %L_365, %L_364, %L_359, %L_533, %L_532, %L_528, %L_331, %L_314, %L_301, %L_299, %L_280, %L_278
  %stackTop.0.be = phi i8* [ %stackTop.32, %L_533 ], [ %stackTop.32, %L_532 ], [ %stackTop.31.ph, %L_528 ], [ %t1200, %L_366 ], [ %t1361, %L_359 ], [ %t1231, %L_365 ], [ %t5005, %L_280 ], [ %stackTop.46, %L_331 ], [ %t4832, %L_299 ], [ %t4737, %L_301 ], [ %stackTop.46, %L_314 ], [ %stackTop.46, %L_278 ], [ %t1284, %L_364 ], [ %stackTop.0.be.ph, %doSwitchNextBlock.backedge.sink.split ]
  %frontier.0.be = phi i8* [ %frontier.32, %L_533 ], [ %frontier.32, %L_532 ], [ %frontier.31.ph, %L_528 ], [ %frontier.14, %L_366 ], [ %frontier.15, %L_359 ], [ %frontier.17, %L_365 ], [ %frontier.46, %L_280 ], [ %frontier.46, %L_331 ], [ %t4802, %L_299 ], [ %frontier.45, %L_301 ], [ %frontier.46, %L_314 ], [ %frontier.46, %L_278 ], [ %frontier.16, %L_364 ], [ %frontier.0.be.ph, %doSwitchNextBlock.backedge.sink.split ]
  %nextBlock.0.be = phi i64 [ %t3170, %L_533 ], [ %t3190, %L_532 ], [ %t3278, %L_528 ], [ %t1204, %L_366 ], [ %t1365, %L_359 ], [ %t1235, %L_365 ], [ %t5009, %L_280 ], [ %t4454, %L_331 ], [ %t4836, %L_299 ], [ %t4741, %L_301 ], [ %t4607, %L_314 ], [ %t5033, %L_278 ], [ %t1288, %L_364 ], [ %t442, %doSwitchNextBlock.backedge.sink.split ]
  br label %doSwitchNextBlock

L_42:                                             ; preds = %flushOut_0
  %t445 = getelementptr inbounds i8, i8* %t565, i64 8
  %t446 = bitcast i8* %t445 to i8**
  %t447 = load i8*, i8** %t446, align 8
  %t451 = bitcast i8* %t565 to i8**
  %t452 = load i8*, i8** %t451, align 8
  %t454 = getelementptr inbounds i8, i8* %frontier.4, i64 8
  %t459 = bitcast i8* %frontier.4 to i64*
  store i64 79, i64* %t459, align 4
  %t461 = getelementptr inbounds i8, i8* %frontier.4, i64 24
  %t464 = bitcast i8* %t454 to i8**
  store i8* %t447, i8** %t464, align 8
  %t467 = getelementptr inbounds i8, i8* %frontier.4, i64 16
  %t468 = bitcast i8* %t467 to i8**
  store i8* %t452, i8** %t468, align 8
  %t471 = getelementptr inbounds i8, i8* %stackTop.4, i64 24
  %t472 = bitcast i8* %t471 to i64*
  store i64 0, i64* %t472, align 4
  %t474 = getelementptr inbounds i8, i8* %stackTop.4, i64 32
  %t478 = load i8*, i8** %t4075, align 8
  %t479 = ptrtoint i8* %t474 to i64
  %t480 = ptrtoint i8* %t478 to i64
  %t481 = sub i64 %t479, %t480
  store i64 %t481, i64* %t4081, align 4
  %t485 = getelementptr inbounds i8, i8* %stackTop.4, i64 48
  %t486 = bitcast i8* %t485 to i8**
  store i8* %t454, i8** %t486, align 8
  %t489 = getelementptr inbounds i8, i8* %stackTop.4, i64 56
  %t490 = bitcast i8* %t489 to i8**
  store i8* %t561, i8** %t490, align 8
  %t493 = getelementptr inbounds i8, i8* %stackTop.4, i64 40
  %t494 = bitcast i8* %t493 to i64*
  store i64 13, i64* %t494, align 4
  br label %L_345

L_41:                                             ; preds = %flushOut_0
  %t543 = bitcast i8* %t542 to i8**
  %t499 = getelementptr inbounds i8, i8* %t565, i64 8
  %t500 = bitcast i8* %t499 to i8**
  %t501 = load i8*, i8** %t500, align 8
  %t505 = bitcast i8* %t565 to i8**
  %t506 = load i8*, i8** %t505, align 8
  %t508 = getelementptr inbounds i8, i8* %frontier.4, i64 8
  %t513 = bitcast i8* %frontier.4 to i64*
  store i64 79, i64* %t513, align 4
  %t515 = getelementptr inbounds i8, i8* %frontier.4, i64 24
  %t518 = bitcast i8* %t508 to i8**
  store i8* %t501, i8** %t518, align 8
  %t521 = getelementptr inbounds i8, i8* %frontier.4, i64 16
  %t522 = bitcast i8* %t521 to i8**
  store i8* %t506, i8** %t522, align 8
  %t527 = getelementptr inbounds i8, i8* %stackTop.4, i64 16
  %t528 = bitcast i8* %t527 to i64*
  %t529 = load i64, i64* %t528, align 4
  store i64 %t529, i64* %t4081, align 4
  store i8* %t508, i8** %t540, align 8
  store i8* %t561, i8** %t543, align 8
  br label %L_345

flushOut_0.sink.split:                            ; preds = %L_78, %L_62
  %t308.sink = phi i8* [ %t308, %L_62 ], [ %t76, %L_78 ]
  %t304551.sink = phi i32* [ %t304551, %L_62 ], [ bitcast (i8* getelementptr (i8, i8* @staticHeapM, i64 8) to i32*), %L_78 ]
  %stackTop.4.ph = phi i8* [ %stackTop.2, %L_62 ], [ %stackTop.1, %L_78 ]
  %frontier.4.ph = phi i8* [ %frontier.2, %L_62 ], [ %frontier.1, %L_78 ]
  %t309 = bitcast i8* %t308.sink to i32*
  %t310 = load i32, i32* %t309, align 4
  store i32 %t310, i32* %t304551.sink, align 4
  br label %flushOut_0

flushOut_0:                                       ; preds = %flushOut_0.sink.split, %loop_0
  %stackTop.4 = phi i8* [ %stackTop.9, %loop_0 ], [ %stackTop.4.ph, %flushOut_0.sink.split ]
  %frontier.4 = phi i8* [ %frontier.9, %loop_0 ], [ %frontier.4.ph, %flushOut_0.sink.split ]
  %t540 = bitcast i8* %stackTop.4 to i8**
  %t542 = getelementptr inbounds i8, i8* %stackTop.4, i64 8
  %1 = bitcast i8* %t542 to i8***
  %t544536 = load i8**, i8*** %1, align 8
  %t547 = load i8*, i8** %t544536, align 8
  store i8* %t547, i8** %t540, align 8
  %t552 = getelementptr inbounds i8, i8* %t547, i64 16
  %2 = bitcast i8* %t552 to i8***
  %t554537 = load i8**, i8*** %2, align 8
  %t559 = getelementptr inbounds i8, i8* %t547, i64 24
  %t560 = bitcast i8* %t559 to i8**
  %t561 = load i8*, i8** %t560, align 8
  %t565 = load i8*, i8** %t554537, align 8
  %t567 = getelementptr inbounds i8, i8* %t565, i64 -8
  %t568 = bitcast i8* %t567 to i64*
  %t569 = load i64, i64* %t568, align 4
  %t571.mask = and i64 %t569, -2
  %switch610 = icmp eq i64 %t571.mask, 118
  br i1 %switch610, label %L_41, label %L_42

L_36:                                             ; preds = %L_35
  %t578 = getelementptr inbounds i8, i8* %stackTop.0, i64 16
  %t579 = bitcast i8* %t578 to i64*
  store i64 172, i64* %t579, align 4
  %t581 = getelementptr inbounds i8, i8* %stackTop.0, i64 24
  store i8* %frontier.0, i8** %t5312, align 8
  store i8* %t581, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t591 = load i8*, i8** %t5312, align 8
  %t594 = load i8*, i8** %t5315, align 8
  %t576 = getelementptr inbounds i8, i8* %t594, i64 -56
  %t597.phi.trans.insert = bitcast i8* %t576 to i8**
  %t598.pre = load i8*, i8** %t597.phi.trans.insert, align 8
  br label %L_11

L_35:                                             ; preds = %doSwitchNextBlock
  %t600 = getelementptr inbounds i8, i8* %stackTop.0, i64 -32
  %t603 = bitcast i8* %t600 to i8**
  %t606 = bitcast i8* %stackTop.0 to i8**
  %t607 = load i8*, i8** %t606, align 8
  store i8* %t607, i8** %t603, align 8
  %t610 = load i8*, i8** %t5411, align 8
  %t612.not = icmp ult i8* %t610, %frontier.0
  br i1 %t612.not, label %L_36, label %L_11

L_26:                                             ; preds = %x_7
  store i64 171, i64* %t707, align 4
  %t621 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  store i8* %frontier.0, i8** %t5312, align 8
  store i8* %t621, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t631 = load i8*, i8** %t5312, align 8
  %t634 = load i8*, i8** %t5315, align 8
  %t616 = getelementptr inbounds i8, i8* %t634, i64 -104
  br label %L_28

L_33:                                             ; preds = %L_32
  %t647 = add i32 %t646, %t689
  store i32 %t647, i32* %t645, align 4
  br label %loop_0

L_32:                                             ; preds = %L_31
  %t644 = getelementptr inbounds i8, i8* %stackTop.8, i64 48
  %t645 = bitcast i8* %t644 to i32*
  %t646 = load i32, i32* %t645, align 4
  %t653 = tail call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %t689, i32 %t646)
  %t654 = extractvalue { i32, i1 } %t653, 1
  br i1 %t654, label %L_11, label %L_33

L_31:                                             ; preds = %L_30
  %t677 = add i64 %t674, %t688
  store i64 %t677, i64* %t671589, align 4
  %t665.not = icmp eq i32 %t689, 0
  br i1 %t665.not, label %L_11, label %L_32

L_30:                                             ; preds = %L_28
  %t669 = getelementptr inbounds i8, i8* %stackTop.8, i64 80
  %3 = bitcast i8* %t669 to i64**
  %t671589 = load i64*, i64** %3, align 8
  %t674 = load i64, i64* %t671589, align 4
  %t680 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %t674, i64 %t688)
  %t681 = extractvalue { i64, i1 } %t680, 1
  br i1 %t681, label %L_11, label %L_31

L_28:                                             ; preds = %x_7, %L_26
  %stackTop.8 = phi i8* [ %t616, %L_26 ], [ %t701, %x_7 ]
  %frontier.8 = phi i8* [ %t631, %L_26 ], [ %frontier.0, %x_7 ]
  %t686 = getelementptr inbounds i8, i8* %stackTop.8, i64 88
  %t687 = bitcast i8* %t686 to i64*
  %t688 = load i64, i64* %t687, align 4
  %t689 = trunc i64 %t688 to i32
  %t691 = sext i32 %t689 to i64
  %t697.not = icmp eq i64 %t688, %t691
  br i1 %t697.not, label %L_30, label %L_11

x_7:                                              ; preds = %doSwitchNextBlock
  %t701 = getelementptr inbounds i8, i8* %stackTop.0, i64 -96
  %t703 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t704 = bitcast i8* %t703 to i64*
  %t707 = bitcast i8* %stackTop.0 to i64*
  %t708 = load i64, i64* %t707, align 4
  store i64 %t708, i64* %t704, align 4
  %t711 = load i8*, i8** %t5411, align 8
  %t713.not = icmp ult i8* %t711, %frontier.0
  br i1 %t713.not, label %L_26, label %L_28

L_25:                                             ; preds = %L_24
  %t717 = getelementptr inbounds i8, i8* %frontier.9, i64 8
  %t722 = bitcast i8* %frontier.9 to i64*
  store i64 93, i64* %t722, align 4
  %t727 = bitcast i8* %t717 to i32*
  store i32 %t812, i32* %t727, align 4
  %t730 = getelementptr inbounds i8, i8* %frontier.9, i64 12
  %t731 = bitcast i8* %t730 to i32*
  %t735 = load i32, i32* %t827.pre-phi, align 4
  store i32 %t735, i32* %t731, align 4
  %t737 = getelementptr inbounds i8, i8* %frontier.9, i64 16
  %t738 = bitcast i8* %t737 to i8**
  %t741 = bitcast i8* %stackTop.9 to i8**
  %t742 = load i8*, i8** %t741, align 8
  store i8* %t742, i8** %t738, align 8
  %t744 = getelementptr inbounds i8, i8* %stackTop.9, i64 96
  %t745 = bitcast i8* %t744 to i8**
  store i8* %t717, i8** %t745, align 8
  %t749 = getelementptr inbounds i8, i8* %stackTop.9, i64 104
  %t750 = bitcast i8* %t749 to i32*
  store i32 1, i32* %t750, align 4
  %t752 = getelementptr inbounds i8, i8* %stackTop.9, i64 88
  br label %L_268.sink.split

L_24:                                             ; preds = %L_23
  %t757 = getelementptr inbounds i8, i8* %stackTop.9, i64 80
  %t758 = bitcast i8* %t757 to i8**
  %t760 = getelementptr inbounds i8, i8* %stackTop.9, i64 64
  %t761 = bitcast i8* %t760 to i8**
  %t762 = load i8*, i8** %t761, align 8
  %t763 = getelementptr inbounds i8, i8* %t762, i64 8
  %t764 = bitcast i8* %t763 to i8**
  %t765 = load i8*, i8** %t764, align 8
  store i8* %t765, i8** %t758, align 8
  %4 = bitcast i8* %t762 to i32**
  %t772586 = load i32*, i32** %4, align 8
  %t776 = load i32, i32* %t772586, align 4
  %switch617 = icmp eq i32 %t776, 0
  br i1 %switch617, label %L_25, label %L_11

L_23:                                             ; preds = %L_22
  %t795 = sext i32 %t812 to i64
  %t783 = sub i64 %t799, %t793
  %t786.not = icmp ult i64 %t783, %t795
  br i1 %t786.not, label %L_11, label %L_24

L_22:                                             ; preds = %L_21
  %t793 = sext i32 %t828 to i64
  %t797 = getelementptr inbounds i8, i8* %stackTop.9, i64 56
  %t798 = bitcast i8* %t797 to i64*
  %t799 = load i64, i64* %t798, align 4
  %t801.not = icmp ult i64 %t799, %t793
  br i1 %t801.not, label %L_11, label %L_23

L_21:                                             ; preds = %loop_0
  %t812 = sub i32 %t832, %t828
  %t821 = tail call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %t832, i32 %t828)
  %t822 = extractvalue { i32, i1 } %t821, 1
  br i1 %t822, label %L_11, label %L_22

loop_0:                                           ; preds = %L_20, %L_33
  %t827.pre-phi = phi i32* [ %t827.phi.trans.insert, %L_20 ], [ %t645, %L_33 ]
  %t828 = phi i32 [ 0, %L_20 ], [ %t647, %L_33 ]
  %stackTop.9 = phi i8* [ %stackTop.10, %L_20 ], [ %stackTop.8, %L_33 ]
  %frontier.9 = phi i8* [ %frontier.10, %L_20 ], [ %frontier.8, %L_33 ]
  %t830 = getelementptr inbounds i8, i8* %stackTop.9, i64 72
  %t831 = bitcast i8* %t830 to i32*
  %t832 = load i32, i32* %t831, align 4
  %t833.not = icmp eq i32 %t832, %t828
  br i1 %t833.not, label %flushOut_0, label %L_21

L_20:                                             ; preds = %L_18
  %t838 = bitcast i8* %t856 to i32*
  store i32 0, i32* %t838, align 4
  %t826.phi.trans.insert = getelementptr inbounds i8, i8* %t430.pn, i64 -16
  %t827.phi.trans.insert = bitcast i8* %t826.phi.trans.insert to i32*
  br label %loop_0

L_18:                                             ; preds = %put_0, %L_16
  %t430.pn = phi i8* [ %t430, %L_16 ], [ %stackTop.0, %put_0 ]
  %frontier.10 = phi i8* [ %t427, %L_16 ], [ %frontier.0, %put_0 ]
  %stackTop.10 = getelementptr inbounds i8, i8* %t430.pn, i64 -64
  %t843 = getelementptr inbounds i8, i8* %t430.pn, i64 -8
  %t844 = bitcast i8* %t843 to i64*
  %t847 = bitcast i8* %stackTop.10 to i8**
  %t848 = load i8*, i8** %t847, align 8
  %t849 = getelementptr inbounds i8, i8* %t848, i64 -16
  %t850 = bitcast i8* %t849 to i64*
  %t851 = load i64, i64* %t850, align 4
  store i64 %t851, i64* %t844, align 4
  %t854 = bitcast i8* %t430.pn to i8**
  %t856 = getelementptr inbounds i8, i8* %t430.pn, i64 -16
  %5 = bitcast i8* %t856 to i8***
  %t858579 = load i8**, i8*** %5, align 8
  %t861 = load i8*, i8** %t858579, align 8
  store i8* %t861, i8** %t854, align 8
  %t866 = trunc i64 %t851 to i32
  %t868 = getelementptr inbounds i8, i8* %t430.pn, i64 8
  %t869 = bitcast i8* %t868 to i32*
  store i32 %t866, i32* %t869, align 4
  %t874 = sext i32 %t866 to i64
  %t880.not = icmp eq i64 %t851, %t874
  br i1 %t880.not, label %L_20, label %L_11

put_0:                                            ; preds = %doSwitchNextBlock, %doSwitchNextBlock
  %t885 = load i8*, i8** %t5411, align 8
  %t887.not = icmp ult i8* %t885, %frontier.0
  br i1 %t887.not, label %L_16, label %L_18

L_15:                                             ; preds = %L_49, %L_14
  %t893 = getelementptr inbounds i8, i8* %stackTop.13, i64 24
  %t894 = bitcast i8* %t893 to i64*
  store i64 14, i64* %t894, align 4
  %t896 = getelementptr inbounds i8, i8* %stackTop.13, i64 32
  %t900 = load i8*, i8** %t4075, align 8
  %t901 = ptrtoint i8* %t896 to i64
  %t902 = ptrtoint i8* %t900 to i64
  %t903 = sub i64 %t901, %t902
  store i64 %t903, i64* %t4081, align 4
  %t908 = bitcast i8* %t1067 to i8**
  store i8* %t1047, i8** %t908, align 8
  %t912 = bitcast i8* %t1077 to i8**
  %t916 = load i8*, i8** %t1120, align 8
  store i8* %t916, i8** %t912, align 8
  %t919 = bitcast i8* %t1034 to i64*
  store i64 16, i64* %t919, align 4
  br label %L_345

L_14:                                             ; preds = %L_13
  %t927.not = icmp slt i32 %t957, %t941
  br i1 %t927.not, label %L_48, label %L_15

L_13:                                             ; preds = %L_12
  %t931 = getelementptr inbounds i8, i8* %stackTop.13, i64 80
  %t932 = bitcast i8* %t931 to i64*
  %t934 = getelementptr inbounds i8, i8* %t1045, i64 -16
  %t935 = bitcast i8* %t934 to i64*
  %t936 = load i64, i64* %t935, align 4
  store i64 %t936, i64* %t932, align 4
  %t941 = trunc i64 %t936 to i32
  %t943 = sext i32 %t941 to i64
  %t949.not = icmp eq i64 %t936, %t943
  br i1 %t949.not, label %L_14, label %L_11

L_12:                                             ; preds = %L_9
  %t957 = add i32 %t1075, %t1090
  %t959 = getelementptr inbounds i8, i8* %stackTop.13, i64 68
  %t960 = bitcast i8* %t959 to i32*
  store i32 %t957, i32* %t960, align 4
  %t966 = tail call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %t1090, i32 %t1075)
  %t967 = extractvalue { i32, i1 } %t966, 1
  br i1 %t967, label %L_11, label %L_13

common.ret:                                       ; preds = %L_1985, %L_1983, %L_1973, %L_1971, %L_1964, %L_1958, %L_1942, %L_531, %L_1939, %L_1937, %L_382, %L_383, %L_1823, %L_409, %L_418, %L_438, %L_510, %L_1880, %L_1875, %L_1873, %L_1871, %L_11
  %common.ret.op = phi i64 [ 61, %L_11 ], [ %t1204, %L_1871 ], [ %t1235, %L_1873 ], [ %t1288, %L_1875 ], [ %t1365, %L_1880 ], [ %t1633, %L_510 ], [ %t2307, %L_438 ], [ %t2541, %L_418 ], [ %t2616, %L_409 ], [ %t2737, %L_1823 ], [ %t2970, %L_383 ], [ %t2995, %L_382 ], [ %t3170, %L_1937 ], [ %t3190, %L_1939 ], [ %t3221, %L_531 ], [ %t3278, %L_1942 ], [ %t4454, %L_1958 ], [ %t4607, %L_1964 ], [ %t4741, %L_1971 ], [ %t4836, %L_1973 ], [ %t5009, %L_1983 ], [ %t5033, %L_1985 ]
  ret i64 %common.ret.op

L_11:                                             ; preds = %L_9, %L_67, %L_65, %L_13, %L_12, %L_18, %L_21, %L_22, %L_23, %L_24, %L_28, %L_66, %L_32, %L_30, %L_31, %L_70, %L_55, %L_54, %L_36, %L_35
  %TP_0.0 = phi i8* [ %t598.pre, %L_36 ], [ %t607, %L_35 ], [ getelementptr (i8, i8* @staticHeapI, i64 9040), %L_9 ], [ getelementptr (i8, i8* @staticHeapI, i64 9040), %L_67 ], [ getelementptr (i8, i8* @staticHeapI, i64 9040), %L_65 ], [ getelementptr (i8, i8* @staticHeapI, i64 9040), %L_13 ], [ getelementptr (i8, i8* @staticHeapI, i64 9024), %L_18 ], [ inttoptr (i64 1 to i8*), %L_28 ], [ inttoptr (i64 1 to i8*), %L_21 ], [ inttoptr (i64 1 to i8*), %L_12 ], [ inttoptr (i64 1 to i8*), %L_66 ], [ inttoptr (i64 1 to i8*), %L_32 ], [ inttoptr (i64 1 to i8*), %L_30 ], [ getelementptr (i8, i8* @staticHeapI, i64 9104), %L_31 ], [ inttoptr (i64 5 to i8*), %L_24 ], [ inttoptr (i64 6 to i8*), %L_23 ], [ inttoptr (i64 6 to i8*), %L_22 ], [ inttoptr (i64 6 to i8*), %L_70 ], [ inttoptr (i64 6 to i8*), %L_55 ], [ inttoptr (i64 6 to i8*), %L_54 ]
  %stackTop.12 = phi i8* [ %t576, %L_36 ], [ %t600, %L_35 ], [ %stackTop.13, %L_9 ], [ %stackTop.13, %L_67 ], [ %stackTop.13, %L_65 ], [ %stackTop.13, %L_13 ], [ %stackTop.10, %L_18 ], [ %stackTop.8, %L_28 ], [ %stackTop.9, %L_21 ], [ %stackTop.13, %L_12 ], [ %stackTop.13, %L_66 ], [ %stackTop.8, %L_32 ], [ %stackTop.8, %L_30 ], [ %stackTop.8, %L_31 ], [ %stackTop.9, %L_24 ], [ %stackTop.9, %L_23 ], [ %stackTop.9, %L_22 ], [ %stackTop.13, %L_70 ], [ %stackTop.3, %L_55 ], [ %stackTop.3, %L_54 ]
  %frontier.11 = phi i8* [ %t591, %L_36 ], [ %frontier.0, %L_35 ], [ %t1054, %L_9 ], [ %t196, %L_67 ], [ %t196, %L_65 ], [ %t1054, %L_13 ], [ %frontier.10, %L_18 ], [ %frontier.8, %L_28 ], [ %frontier.9, %L_21 ], [ %t1054, %L_12 ], [ %t196, %L_66 ], [ %frontier.8, %L_32 ], [ %frontier.8, %L_30 ], [ %frontier.8, %L_31 ], [ %frontier.9, %L_24 ], [ %frontier.9, %L_23 ], [ %frontier.9, %L_22 ], [ %t196, %L_70 ], [ %frontier.3, %L_55 ], [ %frontier.3, %L_54 ]
  %t971 = getelementptr inbounds i8, i8* %stackTop.12, i64 40
  %6 = bitcast i8* %t971 to i8***
  %t973528 = load i8**, i8*** %6, align 8
  %t976 = load i8*, i8** %t973528, align 8
  %t978 = getelementptr inbounds i8, i8* %frontier.11, i64 8
  %t983 = bitcast i8* %frontier.11 to i64*
  store i64 127, i64* %t983, align 4
  %t985 = getelementptr inbounds i8, i8* %frontier.11, i64 32
  %t988 = bitcast i8* %t978 to i8**
  store i8* %TP_0.0, i8** %t988, align 8
  %t991 = getelementptr inbounds i8, i8* %frontier.11, i64 16
  %t992 = bitcast i8* %t991 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4768), i8** %t992, align 8
  %t996 = getelementptr inbounds i8, i8* %frontier.11, i64 24
  %t997 = bitcast i8* %t996 to i8**
  store i8* %t976, i8** %t997, align 8
  %t1002 = getelementptr inbounds i8, i8* %stackTop.12, i64 16
  %t1003 = bitcast i8* %t1002 to i64*
  %t1004 = load i64, i64* %t1003, align 4
  store i64 %t1004, i64* %t4081, align 4
  %t1007 = load i8*, i8** %t4075, align 8
  %t1011 = getelementptr inbounds i8, i8* %t1007, i64 %t1004
  %t1014 = bitcast i8* %t1011 to i8**
  store i8* %t978, i8** %t1014, align 8
  %t1019 = load i8*, i8** %t4075, align 8
  %t1022 = load i64, i64* %t4081, align 4
  %t1023 = getelementptr inbounds i8, i8* %t1019, i64 %t1022
  store i8* %t985, i8** %t5312, align 8
  store i8* %t1023, i8** %t5315, align 8
  br label %common.ret

L_9:                                              ; preds = %L_8
  %t1034 = getelementptr inbounds i8, i8* %stackTop.13, i64 56
  %t1035 = bitcast i8* %t1034 to i8**
  %t1038 = getelementptr inbounds i8, i8* %t1131, i64 8
  %t1039 = bitcast i8* %t1038 to i8**
  %t1040 = load i8*, i8** %t1039, align 8
  store i8* %t1040, i8** %t1035, align 8
  %t1044 = bitcast i8* %t1131 to i8**
  %t1045 = load i8*, i8** %t1044, align 8
  %t1047 = getelementptr inbounds i8, i8* %frontier.13, i64 8
  %t1052 = bitcast i8* %frontier.13 to i64*
  store i64 79, i64* %t1052, align 4
  %t1054 = getelementptr inbounds i8, i8* %frontier.13, i64 24
  %t1057 = bitcast i8* %t1047 to i8**
  %t1061 = load i8*, i8** %t1035, align 8
  store i8* %t1061, i8** %t1057, align 8
  %t1063 = getelementptr inbounds i8, i8* %frontier.13, i64 16
  %t1064 = bitcast i8* %t1063 to i8**
  store i8* %t1045, i8** %t1064, align 8
  %t1067 = getelementptr inbounds i8, i8* %stackTop.13, i64 64
  %t1068 = bitcast i8* %t1067 to i32*
  %7 = bitcast i8* %t1034 to i32**
  %t1072539 = load i32*, i32** %7, align 8
  %t1075 = load i32, i32* %t1072539, align 4
  store i32 %t1075, i32* %t1068, align 4
  %t1077 = getelementptr inbounds i8, i8* %stackTop.13, i64 72
  %t1078 = bitcast i8* %t1077 to i64*
  %t1081 = bitcast i8* %stackTop.13 to i8**
  %t1082 = load i8*, i8** %t1081, align 8
  %t1083 = getelementptr inbounds i8, i8* %t1082, i64 -16
  %t1084 = bitcast i8* %t1083 to i64*
  %t1085 = load i64, i64* %t1084, align 4
  store i64 %t1085, i64* %t1078, align 4
  %t1090 = trunc i64 %t1085 to i32
  %t1092 = sext i32 %t1090 to i64
  %t1098.not = icmp eq i64 %t1085, %t1092
  br i1 %t1098.not, label %L_12, label %L_11

L_8:                                              ; preds = %L_5, %L_6
  %stackTop.13 = phi i8* [ %t2, %L_6 ], [ %stackTop.0, %L_5 ]
  %frontier.13 = phi i8* [ %t17, %L_6 ], [ %frontier.0, %L_5 ]
  %t1102 = getelementptr inbounds i8, i8* %stackTop.13, i64 40
  %t1103 = bitcast i8* %t1102 to i8**
  %t1105 = getelementptr inbounds i8, i8* %stackTop.13, i64 8
  %8 = bitcast i8* %t1105 to i8***
  %t1107523 = load i8**, i8*** %8, align 8
  %t1110 = load i8*, i8** %t1107523, align 8
  store i8* %t1110, i8** %t1103, align 8
  %t1115 = getelementptr inbounds i8, i8* %t1110, i64 16
  %9 = bitcast i8* %t1115 to i8***
  %t1117524 = load i8**, i8*** %9, align 8
  %t1119 = getelementptr inbounds i8, i8* %stackTop.13, i64 48
  %t1120 = bitcast i8* %t1119 to i8**
  %t1125 = getelementptr inbounds i8, i8* %t1110, i64 24
  %t1126 = bitcast i8* %t1125 to i8**
  %t1127 = load i8*, i8** %t1126, align 8
  store i8* %t1127, i8** %t1120, align 8
  %t1131 = load i8*, i8** %t1117524, align 8
  %t1133 = getelementptr inbounds i8, i8* %t1131, i64 -8
  %t1134 = bitcast i8* %t1133 to i64*
  %t1135 = load i64, i64* %t1134, align 4
  %t1137.mask = and i64 %t1135, -2
  %switch628 = icmp eq i64 %t1137.mask, 118
  br i1 %switch628, label %L_9, label %L_65

L_5:                                              ; preds = %L_1828
  %t1141 = load i8*, i8** %t5411, align 8
  %t1143.not = icmp ult i8* %t1141, %frontier.0
  br i1 %t1143.not, label %L_6, label %L_8

L_1828:                                           ; preds = %doSwitchNextBlock, %L_1765
  %t1147 = getelementptr inbounds i8, i8* %stackTop.0, i64 16
  %t1148 = bitcast i8* %t1147 to i64*
  %t1151 = load i64, i64* %t4081, align 4
  store i64 %t1151, i64* %t1148, align 4
  %t1154 = load i8*, i8** %t4312, align 8
  %t1156.not = icmp ult i8* %t1154, %stackTop.0
  br i1 %t1156.not, label %L_6, label %L_5

L_347:                                            ; preds = %L_345, %L_346
  %t1162 = getelementptr inbounds i8, i8* %stackTop.18, i64 16
  %t1163 = bitcast i8* %t1162 to i64*
  store i64 149, i64* %t1163, align 4
  %t1165 = getelementptr inbounds i8, i8* %stackTop.18, i64 24
  store i8* %frontier.19, i8** %t5312, align 8
  store i8* %t1165, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t1175 = load i8*, i8** %t5312, align 8
  %t1178 = load i8*, i8** %t5315, align 8
  %t1160 = getelementptr inbounds i8, i8* %t1178, i64 -24
  br label %L_349

L_367:                                            ; preds = %loop_13
  %t1180 = getelementptr inbounds i8, i8* %stackTop.16, i64 -8
  br label %doSwitchNextBlock.backedge.sink.split

L_366:                                            ; preds = %L_349, %L_352, %L_353
  %frontier.14 = phi i8* [ %frontier.18, %L_349 ], [ %frontier.17, %L_352 ], [ %frontier.17, %L_353 ]
  %t1185 = load i8*, i8** %t4075, align 8
  %t1188 = load i64, i64* %t4081, align 4
  %t1189 = getelementptr inbounds i8, i8* %t1185, i64 %t1188
  %t1192 = bitcast i8* %t1189 to i8**
  store i8* inttoptr (i64 6 to i8*), i8** %t1192, align 8
  %t1196 = load i8*, i8** %t4075, align 8
  %t1199 = load i64, i64* %t4081, align 4
  %t1200 = getelementptr inbounds i8, i8* %t1196, i64 %t1199
  %t1202 = getelementptr inbounds i8, i8* %t1200, i64 -8
  %t1203 = bitcast i8* %t1202 to i64*
  %t1204 = load i64, i64* %t1203, align 4
  %t1205 = getelementptr inbounds [84 x i64 (i8*, i8*, i8*, i64)*], [84 x i64 (i8*, i8*, i8*, i64)*]* @nextChunks, i64 0, i64 %t1204
  %t1206 = load i64 (i8*, i8*, i8*, i64)*, i64 (i8*, i8*, i8*, i64)** %t1205, align 8
  %t1207 = icmp eq i64 (i8*, i8*, i8*, i64)* %t1206, @Chunk_11
  br i1 %t1207, label %doSwitchNextBlock.backedge, label %L_1871

L_1871:                                           ; preds = %L_366
  store i8* %frontier.14, i8** %t5312, align 8
  store i8* %t1200, i8** %t5315, align 8
  br label %common.ret

L_365:                                            ; preds = %L_354
  %t1216 = load i8*, i8** %t4075, align 8
  %t1219 = load i64, i64* %t4081, align 4
  %t1220 = getelementptr inbounds i8, i8* %t1216, i64 %t1219
  %t1223 = bitcast i8* %t1220 to i8**
  store i8* inttoptr (i64 5 to i8*), i8** %t1223, align 8
  %t1227 = load i8*, i8** %t4075, align 8
  %t1230 = load i64, i64* %t4081, align 4
  %t1231 = getelementptr inbounds i8, i8* %t1227, i64 %t1230
  %t1233 = getelementptr inbounds i8, i8* %t1231, i64 -8
  %t1234 = bitcast i8* %t1233 to i64*
  %t1235 = load i64, i64* %t1234, align 4
  %t1236 = getelementptr inbounds [84 x i64 (i8*, i8*, i8*, i64)*], [84 x i64 (i8*, i8*, i8*, i64)*]* @nextChunks, i64 0, i64 %t1235
  %t1237 = load i64 (i8*, i8*, i8*, i64)*, i64 (i8*, i8*, i8*, i64)** %t1236, align 8
  %t1238 = icmp eq i64 (i8*, i8*, i8*, i64)* %t1237, @Chunk_11
  br i1 %t1238, label %doSwitchNextBlock.backedge, label %L_1873

L_1873:                                           ; preds = %L_365
  store i8* %frontier.17, i8** %t5312, align 8
  store i8* %t1231, i8** %t5315, align 8
  br label %common.ret

L_356:                                            ; preds = %x_1
  store i64 165, i64* %t1397, align 4
  %t1251 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  store i8* %frontier.0, i8** %t5312, align 8
  store i8* %t1251, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t1261 = load i8*, i8** %t5312, align 8
  %t1264 = load i8*, i8** %t5315, align 8
  %t1246 = getelementptr inbounds i8, i8* %t1264, i64 -64
  br label %L_358

L_364:                                            ; preds = %L_361
  %t1267 = load i8*, i8** %t4075, align 8
  %t1270 = load i64, i64* %t4081, align 4
  %t1271 = getelementptr inbounds i8, i8* %t1267, i64 %t1270
  %t1274 = bitcast i8* %t1271 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 9104), i8** %t1274, align 8
  %t1280 = load i8*, i8** %t4075, align 8
  %t1283 = load i64, i64* %t4081, align 4
  %t1284 = getelementptr inbounds i8, i8* %t1280, i64 %t1283
  %t1286 = getelementptr inbounds i8, i8* %t1284, i64 -8
  %t1287 = bitcast i8* %t1286 to i64*
  %t1288 = load i64, i64* %t1287, align 4
  %t1289 = getelementptr inbounds [84 x i64 (i8*, i8*, i8*, i64)*], [84 x i64 (i8*, i8*, i8*, i64)*]* @nextChunks, i64 0, i64 %t1288
  %t1290 = load i64 (i8*, i8*, i8*, i64)*, i64 (i8*, i8*, i8*, i64)** %t1289, align 8
  %t1291 = icmp eq i64 (i8*, i8*, i8*, i64)* %t1290, @Chunk_11
  br i1 %t1291, label %doSwitchNextBlock.backedge, label %L_1875

L_1875:                                           ; preds = %L_364
  store i8* %frontier.16, i8** %t5312, align 8
  store i8* %t1284, i8** %t5315, align 8
  br label %common.ret

L_363:                                            ; preds = %L_362
  %t1307 = add i32 %t1306, %t1379
  store i32 %t1307, i32* %t1305, align 4
  %t1520.phi.trans.insert = getelementptr inbounds i8, i8* %stackTop.15, i64 24
  %t1521.phi.trans.insert = bitcast i8* %t1520.phi.trans.insert to i32*
  %t1522.pre = load i32, i32* %t1521.phi.trans.insert, align 4
  br label %loop_13

L_362:                                            ; preds = %L_361
  %t1304 = getelementptr inbounds i8, i8* %stackTop.15, i64 8
  %t1305 = bitcast i8* %t1304 to i32*
  %t1306 = load i32, i32* %t1305, align 4
  %t1313 = tail call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %t1379, i32 %t1306)
  %t1314 = extractvalue { i32, i1 } %t1313, 1
  br i1 %t1314, label %L_359, label %L_363

L_361:                                            ; preds = %L_360
  %t1337 = add i64 %t1334, %t1378
  store i64 %t1337, i64* %t1331574, align 4
  %t1325.not = icmp eq i32 %t1379, 0
  br i1 %t1325.not, label %L_364, label %L_362

L_360:                                            ; preds = %L_358
  %t1329 = getelementptr inbounds i8, i8* %stackTop.15, i64 40
  %10 = bitcast i8* %t1329 to i64**
  %t1331574 = load i64*, i64** %10, align 8
  %t1334 = load i64, i64* %t1331574, align 4
  %t1340 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %t1334, i64 %t1378)
  %t1341 = extractvalue { i64, i1 } %t1340, 1
  br i1 %t1341, label %L_359, label %L_361

L_359:                                            ; preds = %L_351, %L_358, %L_360, %L_362
  %frontier.15 = phi i8* [ %frontier.17, %L_351 ], [ %frontier.16, %L_360 ], [ %frontier.16, %L_362 ], [ %frontier.16, %L_358 ]
  %t1346 = load i8*, i8** %t4075, align 8
  %t1349 = load i64, i64* %t4081, align 4
  %t1350 = getelementptr inbounds i8, i8* %t1346, i64 %t1349
  %t1353 = bitcast i8* %t1350 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t1353, align 8
  %t1357 = load i8*, i8** %t4075, align 8
  %t1360 = load i64, i64* %t4081, align 4
  %t1361 = getelementptr inbounds i8, i8* %t1357, i64 %t1360
  %t1363 = getelementptr inbounds i8, i8* %t1361, i64 -8
  %t1364 = bitcast i8* %t1363 to i64*
  %t1365 = load i64, i64* %t1364, align 4
  %t1366 = getelementptr inbounds [84 x i64 (i8*, i8*, i8*, i64)*], [84 x i64 (i8*, i8*, i8*, i64)*]* @nextChunks, i64 0, i64 %t1365
  %t1367 = load i64 (i8*, i8*, i8*, i64)*, i64 (i8*, i8*, i8*, i64)** %t1366, align 8
  %t1368 = icmp eq i64 (i8*, i8*, i8*, i64)* %t1367, @Chunk_11
  br i1 %t1368, label %doSwitchNextBlock.backedge, label %L_1880

L_1880:                                           ; preds = %L_359
  store i8* %frontier.15, i8** %t5312, align 8
  store i8* %t1361, i8** %t5315, align 8
  br label %common.ret

L_358:                                            ; preds = %x_1, %L_356
  %stackTop.15 = phi i8* [ %t1246, %L_356 ], [ %t1391, %x_1 ]
  %frontier.16 = phi i8* [ %t1261, %L_356 ], [ %frontier.0, %x_1 ]
  %t1376 = getelementptr inbounds i8, i8* %stackTop.15, i64 48
  %t1377 = bitcast i8* %t1376 to i64*
  %t1378 = load i64, i64* %t1377, align 4
  %t1379 = trunc i64 %t1378 to i32
  %t1381 = sext i32 %t1379 to i64
  %t1387.not = icmp eq i64 %t1378, %t1381
  br i1 %t1387.not, label %L_360, label %L_359

x_1:                                              ; preds = %doSwitchNextBlock
  %t1391 = getelementptr inbounds i8, i8* %stackTop.0, i64 -56
  %t1393 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t1394 = bitcast i8* %t1393 to i64*
  %t1397 = bitcast i8* %stackTop.0 to i64*
  %t1398 = load i64, i64* %t1397, align 4
  store i64 %t1398, i64* %t1394, align 4
  %t1401 = load i8*, i8** %t5411, align 8
  %t1403.not = icmp ult i8* %t1401, %frontier.0
  br i1 %t1403.not, label %L_356, label %L_358

L_355:                                            ; preds = %L_354
  %t1407 = getelementptr inbounds i8, i8* %frontier.17, i64 8
  %t1412 = bitcast i8* %frontier.17 to i64*
  store i64 95, i64* %t1412, align 4
  %t1417 = bitcast i8* %t1407 to i32*
  store i32 %t1502, i32* %t1417, align 4
  %t1420 = getelementptr inbounds i8, i8* %frontier.17, i64 12
  %t1421 = bitcast i8* %t1420 to i32*
  %t1425 = load i32, i32* %t1517.pre-phi, align 4
  store i32 %t1425, i32* %t1421, align 4
  %t1427 = getelementptr inbounds i8, i8* %frontier.17, i64 16
  %t1428 = bitcast i8* %t1427 to i8**
  %t1430 = getelementptr inbounds i8, i8* %stackTop.16, i64 16
  %t1431 = bitcast i8* %t1430 to i8**
  %t1432 = load i8*, i8** %t1431, align 8
  store i8* %t1432, i8** %t1428, align 8
  %t1434 = getelementptr inbounds i8, i8* %stackTop.16, i64 56
  %t1435 = bitcast i8* %t1434 to i8**
  store i8* %t1407, i8** %t1435, align 8
  %t1439 = getelementptr inbounds i8, i8* %stackTop.16, i64 64
  %t1440 = bitcast i8* %t1439 to i32*
  store i32 1, i32* %t1440, align 4
  %t1442 = getelementptr inbounds i8, i8* %stackTop.16, i64 48
  br label %L_268.sink.split

L_354:                                            ; preds = %L_353
  %t1447 = getelementptr inbounds i8, i8* %stackTop.16, i64 40
  %t1448 = bitcast i8* %t1447 to i8**
  %t1451 = bitcast i8* %stackTop.16 to i8**
  %t1452 = load i8*, i8** %t1451, align 8
  %t1453 = getelementptr inbounds i8, i8* %t1452, i64 8
  %t1454 = bitcast i8* %t1453 to i8**
  %t1455 = load i8*, i8** %t1454, align 8
  store i8* %t1455, i8** %t1448, align 8
  %11 = bitcast i8* %t1452 to i32**
  %t1462499 = load i32*, i32** %11, align 8
  %t1466 = load i32, i32* %t1462499, align 4
  %switch636 = icmp eq i32 %t1466, 0
  br i1 %switch636, label %L_355, label %L_365

L_353:                                            ; preds = %L_352
  %t1485 = sext i32 %t1502 to i64
  %t1473 = sub i64 %t1489, %t1483
  %t1476.not = icmp ult i64 %t1473, %t1485
  br i1 %t1476.not, label %L_366, label %L_354

L_352:                                            ; preds = %L_351
  %t1483 = sext i32 %t1518 to i64
  %t1487 = getelementptr inbounds i8, i8* %stackTop.16, i64 32
  %t1488 = bitcast i8* %t1487 to i64*
  %t1489 = load i64, i64* %t1488, align 4
  %t1491.not = icmp ult i64 %t1489, %t1483
  br i1 %t1491.not, label %L_366, label %L_353

L_351:                                            ; preds = %loop_13
  %t1502 = sub i32 %t1522, %t1518
  %t1511 = tail call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %t1522, i32 %t1518)
  %t1512 = extractvalue { i32, i1 } %t1511, 1
  br i1 %t1512, label %L_359, label %L_352

loop_13:                                          ; preds = %L_350, %L_363
  %t1517.pre-phi = phi i32* [ %t1538, %L_350 ], [ %t1305, %L_363 ]
  %t1522 = phi i32 [ %t1579, %L_350 ], [ %t1522.pre, %L_363 ]
  %t1518 = phi i32 [ 0, %L_350 ], [ %t1307, %L_363 ]
  %stackTop.16 = phi i8* [ %stackTop.17, %L_350 ], [ %stackTop.15, %L_363 ]
  %frontier.17 = phi i8* [ %frontier.18, %L_350 ], [ %frontier.16, %L_363 ]
  %t1523.not = icmp eq i32 %t1522, %t1518
  br i1 %t1523.not, label %L_367, label %L_351

L_350:                                            ; preds = %L_349
  %t1530 = getelementptr inbounds i8, i8* %stackTop.17, i64 8
  %t1531 = bitcast i8* %t1530 to i8**
  %t1532 = load i8*, i8** %t1531, align 8
  %t1533 = getelementptr inbounds i8, i8* %t1532, i64 8
  %t1534 = bitcast i8* %t1533 to i8**
  %t1535 = load i8*, i8** %t1534, align 8
  store i8* %t1535, i8** %t1541, align 8
  %t1538 = bitcast i8* %t1530 to i32*
  store i32 0, i32* %t1538, align 4
  br label %loop_13

L_349:                                            ; preds = %L_346, %L_347
  %stackTop.17 = phi i8* [ %t1160, %L_347 ], [ %stackTop.18, %L_346 ]
  %frontier.18 = phi i8* [ %t1175, %L_347 ], [ %frontier.19, %L_346 ]
  %t1541 = bitcast i8* %stackTop.17 to i8**
  %t1542 = load i8*, i8** %t1541, align 8
  %12 = bitcast i8* %t1542 to i32**
  %t1545492 = load i32*, i32** %12, align 8
  %t1547 = getelementptr inbounds i8, i8* %stackTop.17, i64 16
  %t1548 = bitcast i8* %t1547 to i8**
  %t1553 = getelementptr inbounds i8, i8* %t1542, i64 8
  %t1554 = bitcast i8* %t1553 to i8**
  %t1555 = load i8*, i8** %t1554, align 8
  store i8* %t1555, i8** %t1548, align 8
  %t1557 = getelementptr inbounds i8, i8* %stackTop.17, i64 24
  %t1558 = bitcast i8* %t1557 to i32*
  %t1562 = load i32, i32* %t1545492, align 4
  store i32 %t1562, i32* %t1558, align 4
  store i32 0, i32* %t1545492, align 4
  %t1567 = getelementptr inbounds i8, i8* %stackTop.17, i64 32
  %t1568 = bitcast i8* %t1567 to i64*
  %t1572 = load i8*, i8** %t1548, align 8
  %t1573 = getelementptr inbounds i8, i8* %t1572, i64 -16
  %t1574 = bitcast i8* %t1573 to i64*
  %t1575 = load i64, i64* %t1574, align 4
  store i64 %t1575, i64* %t1568, align 4
  %t1579 = load i32, i32* %t1558, align 4
  %t1580 = sext i32 %t1579 to i64
  %t1586.not = icmp ult i64 %t1575, %t1580
  br i1 %t1586.not, label %L_366, label %L_350

L_346:                                            ; preds = %L_345
  %t1591 = load i8*, i8** %t5411, align 8
  %t1593.not = icmp ult i8* %t1591, %frontier.19
  br i1 %t1593.not, label %L_347, label %L_349

L_345:                                            ; preds = %doSwitchNextBlock, %L_69, %L_42, %L_41, %L_15, %L_442, %L_429, %L_1267, %L_1259
  %stackTop.18 = phi i8* [ %t2244, %L_442 ], [ %t2429, %L_429 ], [ %t485, %L_42 ], [ %stackTop.4, %L_41 ], [ %t130, %L_69 ], [ %t1067, %L_15 ], [ %t5135.pn, %L_1267 ], [ %t5135.pn, %L_1259 ], [ %stackTop.0, %doSwitchNextBlock ]
  %frontier.19 = phi i8* [ %t2405, %L_442 ], [ %t2405, %L_429 ], [ %t461, %L_42 ], [ %t515, %L_41 ], [ %t196, %L_69 ], [ %t1054, %L_15 ], [ %t5219, %L_1267 ], [ %t5219, %L_1259 ], [ %frontier.0, %doSwitchNextBlock ]
  %t1598 = load i8*, i8** %t4312, align 8
  %t1600.not = icmp ult i8* %t1598, %stackTop.18
  br i1 %t1600.not, label %L_347, label %L_346

L_369:                                            ; preds = %L_1821
  %t1606 = getelementptr inbounds i8, i8* %stackTop.30, i64 40
  %t1607 = bitcast i8* %t1606 to i64*
  store i64 157, i64* %t1607, align 4
  %t1609 = getelementptr inbounds i8, i8* %stackTop.30, i64 48
  store i8* %frontier.0, i8** %t5312, align 8
  store i8* %t1609, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t1619 = load i8*, i8** %t5312, align 8
  %t1622 = load i8*, i8** %t5315, align 8
  %t1604 = getelementptr inbounds i8, i8* %t1622, i64 -48
  br label %L_371

L_510:                                            ; preds = %L_371
  %t1625 = load i8*, i8** %t4075, align 8
  %t1628 = load i64, i64* %t4081, align 4
  %t1629 = getelementptr inbounds i8, i8* %t1625, i64 %t1628
  %t1631 = getelementptr inbounds i8, i8* %t1629, i64 -8
  %t1632 = bitcast i8* %t1631 to i64*
  %t1633 = load i64, i64* %t1632, align 4
  store i8* %frontier.30, i8** %t5312, align 8
  store i8* %t1629, i8** %t5315, align 8
  br label %common.ret

L_421.backedge:                                   ; preds = %x_2, %L_460, %L_462, %L_478, %L_480
  %t1664.sink1271 = phi i8* [ %t1829, %L_480 ], [ %t1853, %L_478 ], [ %t2018, %L_462 ], [ %t2042, %L_460 ], [ %t1664, %x_2 ]
  %t1646 = getelementptr inbounds i8, i8* %t1664.sink1271, i64 8
  %t1647 = bitcast i8* %t1646 to i8**
  %t1648 = load i8*, i8** %t1647, align 8
  %t1654 = bitcast i8* %t1664.sink1271 to i8**
  %t1655 = load i8*, i8** %t1654, align 8
  store i8* %t1648, i8** %t1852, align 8
  br label %L_421

x_2:                                              ; preds = %L_421
  %t1664 = load i8*, i8** %t1852, align 8
  %cond35 = icmp eq i8* %t1664, inttoptr (i64 1 to i8*)
  br i1 %cond35, label %L_438, label %L_421.backedge

L_507:                                            ; preds = %L_451
  %cond23 = icmp eq i8* %TP_0.1, inttoptr (i64 1 to i8*)
  br i1 %cond23, label %L_460, label %L_451.backedge

L_505:                                            ; preds = %L_452
  %cond24 = icmp eq i8* %TP_0.1, inttoptr (i64 1 to i8*)
  br i1 %cond24, label %L_460, label %L_451.backedge

L_501:                                            ; preds = %L_494
  %t1705 = tail call i32 @Posix_Error_getErrno()
  %t1700 = load i32, i32* %t2079, align 4
  %t1702.not = icmp eq i32 %t1700, 0
  br i1 %t1702.not, label %L_462, label %loop_17

L_498:                                            ; preds = %L_497
  %t1719 = add i32 %t1727, -1
  store i32 %t1719, i32* %t2079, align 4
  %cond26 = icmp eq i8* %TP_0.1, inttoptr (i64 1 to i8*)
  br i1 %cond26, label %L_460, label %L_451.backedge

L_497:                                            ; preds = %L_494
  %t1727 = load i32, i32* %t2079, align 4
  %t1729.not = icmp eq i32 %t1727, 0
  br i1 %t1729.not, label %L_462, label %L_498

L_494:                                            ; preds = %L_482
  br i1 %t1734.not, label %L_501, label %L_497

L_492:                                            ; preds = %L_490
  %t1748 = add i32 %t1755, -1
  store i32 %t1748, i32* %t2079, align 4
  br label %errorMsg_0

L_490:                                            ; preds = %L_483
  %t1760 = tail call i32 @Posix_Error_getErrno()
  %t1755 = load i32, i32* %t2079, align 4
  %t1757.not = icmp eq i32 %t1755, 0
  br i1 %t1757.not, label %L_462, label %L_492

L_487:                                            ; preds = %L_486
  %t1774 = add i32 %t1782, -1
  store i32 %t1774, i32* %t2079, align 4
  %cond31 = icmp eq i8* %TP_0.1, inttoptr (i64 1 to i8*)
  br i1 %cond31, label %L_460, label %L_451.backedge

L_486:                                            ; preds = %L_483
  %t1782 = load i32, i32* %t2079, align 4
  %t1784.not = icmp eq i32 %t1782, 0
  br i1 %t1784.not, label %L_462, label %L_487

L_483:                                            ; preds = %L_482
  br i1 %t1734.not, label %L_490, label %L_486

L_482:                                            ; preds = %loop_17
  %t1804.not = icmp eq i32 %storemerge, 0
  store i32 %storemerge.in, i32* %t2079, align 4
  %t1744 = tail call i32 @Posix_IO_close(i32 0)
  %t1734.not = icmp eq i32 %t1744, -1
  br i1 %t1804.not, label %L_494, label %L_483

L_480:                                            ; preds = %errorMsg_0
  %t1829 = load i8*, i8** %t1852, align 8
  %cond27 = icmp eq i8* %t1829, inttoptr (i64 1 to i8*)
  br i1 %cond27, label %L_438, label %L_421.backedge

L_478:                                            ; preds = %L_1827
  %t1853 = load i8*, i8** %t1852, align 8
  %cond29 = icmp eq i8* %t1853, inttoptr (i64 1 to i8*)
  br i1 %cond29, label %L_438, label %L_421.backedge

L_477:                                            ; preds = %L_477.preheader1434, %L_477
  %TW64_1.1872 = phi i64 [ %t1874, %L_477 ], [ %TW64_1.1872.ph, %L_477.preheader1434 ]
  %13 = shl i64 %TW64_1.1872, 32
  %t1858 = ashr exact i64 %13, 32
  %t1862 = getelementptr inbounds i8, i8* %t1915, i64 %t1858
  %t1865 = load i8, i8* %t1862, align 1
  %t1869 = getelementptr inbounds i8, i8* %t1931, i64 %TW64_1.1872
  store i8 %t1865, i8* %t1869, align 1
  %t1874 = add nuw nsw i64 %TW64_1.1872, 1
  %t1902.not = icmp slt i64 %t1874, %t1911
  br i1 %t1902.not, label %L_477, label %L_475, !llvm.loop !0

L_421.outer:                                      ; preds = %L_421.outer.preheader, %L_475
  %t2171.sink1273.sink = phi i8* [ %t1898, %L_475 ], [ %t2171.sink1273.sink.ph, %L_421.outer.preheader ]
  %t2170.sink.sink = phi i8** [ %t1897, %L_475 ], [ %t2170.sink.sink.ph, %L_421.outer.preheader ]
  %stackTop.24.ph729 = phi i8* [ %t1906, %L_475 ], [ %stackTop.24.ph729.ph, %L_421.outer.preheader ]
  %frontier.25.ph730 = phi i8* [ %t1934, %L_475 ], [ %frontier.25.ph730.ph, %L_421.outer.preheader ]
  %t2151 = getelementptr inbounds i8, i8* %t2171.sink1273.sink, i64 8
  %t2152 = bitcast i8* %t2151 to i8**
  %t2153 = load i8*, i8** %t2152, align 8
  %t2159 = bitcast i8* %t2171.sink1273.sink to i8**
  %t2160 = load i8*, i8** %t2159, align 8
  store i8* %t2153, i8** %t2170.sink.sink, align 8
  %t1939 = getelementptr inbounds i8, i8* %stackTop.24.ph729, i64 40
  %t1940 = bitcast i8* %t1939 to i64*
  %t1943 = getelementptr inbounds i8, i8* %stackTop.24.ph729, i64 48
  %t1944 = bitcast i8* %t1943 to i8**
  %t1851 = getelementptr inbounds i8, i8* %stackTop.24.ph729, i64 8
  %t1852 = bitcast i8* %t1851 to i8**
  br label %L_421

L_475:                                            ; preds = %L_477, %middle.block1418, %L_473
  %t1896 = getelementptr inbounds i8, i8* %t1937, i64 -56
  %t1897 = bitcast i8* %t1896 to i8**
  %t1898 = load i8*, i8** %t1897, align 8
  %cond30 = icmp eq i8* %t1898, inttoptr (i64 1 to i8*)
  br i1 %cond30, label %L_438, label %L_421.outer

L_473:                                            ; preds = %L_1827
  %t1921 = getelementptr inbounds i8, i8* %stackTop.24.ph729, i64 56
  %t1922 = bitcast i8* %t1921 to i64*
  store i64 164, i64* %t1922, align 4
  %t1924 = getelementptr inbounds i8, i8* %stackTop.24.ph729, i64 64
  store i8* %frontier.25.ph730, i8** %t5312, align 8
  store i8* %t1924, i8** %t5315, align 8
  %t1931 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t1957, i64 21)
  %t1934 = load i8*, i8** %t5312, align 8
  %t1937 = load i8*, i8** %t5315, align 8
  %t1906 = getelementptr inbounds i8, i8* %t1937, i64 -64
  %t1909 = getelementptr inbounds i8, i8* %t1937, i64 -24
  %t1910 = bitcast i8* %t1909 to i64*
  %t1911 = load i64, i64* %t1910, align 4
  %t1913 = getelementptr inbounds i8, i8* %t1937, i64 -16
  %t1914 = bitcast i8* %t1913 to i8**
  %t1915 = load i8*, i8** %t1914, align 8
  %t1902.not871 = icmp sgt i64 %t1911, 0
  br i1 %t1902.not871, label %L_477.preheader, label %L_475

L_477.preheader:                                  ; preds = %L_473
  %14 = add i64 %t1911, -4
  %15 = icmp ult i64 %14, 2147483645
  br i1 %15, label %vector.memcheck1412, label %L_477.preheader1434

vector.memcheck1412:                              ; preds = %L_477.preheader
  %scevgep1413 = getelementptr i8, i8* %t1931, i64 %t1911
  %scevgep1414 = getelementptr i8, i8* %t1915, i64 %t1911
  %bound01415 = icmp ult i8* %t1931, %scevgep1414
  %bound11416 = icmp ult i8* %t1915, %scevgep1413
  %found.conflict1417 = and i1 %bound01415, %bound11416
  br i1 %found.conflict1417, label %L_477.preheader1434, label %vector.ph1422

vector.ph1422:                                    ; preds = %vector.memcheck1412
  %n.vec1424 = and i64 %t1911, -4
  br label %vector.body1420

vector.body1420:                                  ; preds = %vector.body1420, %vector.ph1422
  %index1427 = phi i64 [ 0, %vector.ph1422 ], [ %index.next1429, %vector.body1420 ]
  %16 = shl i64 %index1427, 32
  %17 = ashr exact i64 %16, 32
  %18 = getelementptr inbounds i8, i8* %t1915, i64 %17
  %19 = bitcast i8* %18 to <4 x i8>*
  %wide.load1428 = load <4 x i8>, <4 x i8>* %19, align 1, !alias.scope !2
  %20 = getelementptr inbounds i8, i8* %t1931, i64 %index1427
  %21 = bitcast i8* %20 to <4 x i8>*
  store <4 x i8> %wide.load1428, <4 x i8>* %21, align 1, !alias.scope !5, !noalias !2
  %index.next1429 = add nuw i64 %index1427, 4
  %22 = icmp eq i64 %index.next1429, %n.vec1424
  br i1 %22, label %middle.block1418, label %vector.body1420, !llvm.loop !7

middle.block1418:                                 ; preds = %vector.body1420
  %cmp.n1426 = icmp eq i64 %t1911, %n.vec1424
  br i1 %cmp.n1426, label %L_475, label %L_477.preheader1434

L_477.preheader1434:                              ; preds = %vector.memcheck1412, %L_477.preheader, %middle.block1418
  %TW64_1.1872.ph = phi i64 [ 0, %vector.memcheck1412 ], [ 0, %L_477.preheader ], [ %n.vec1424, %middle.block1418 ]
  br label %L_477

L_1827:                                           ; preds = %loop_18
  store i64 %t1957, i64* %t1940, align 4
  store i8* %t1969, i8** %t1944, align 8
  %trunc483 = icmp sgt i32 %TW32_0.0, -1
  br i1 %trunc483, label %L_473, label %L_478

loop_18:                                          ; preds = %errorMsg_0, %loop_18
  %TW32_0.0 = phi i32 [ %t1954, %loop_18 ], [ 0, %errorMsg_0 ]
  %t1957 = sext i32 %TW32_0.0 to i64
  %t1961 = getelementptr inbounds i8, i8* %t1969, i64 %t1957
  %t1964 = load i8, i8* %t1961, align 1
  %cond28 = icmp eq i8 %t1964, 0
  %t1954 = add i32 %TW32_0.0, 1
  br i1 %cond28, label %L_1827, label %loop_18

errorMsg_0:                                       ; preds = %loop_17, %L_492
  %TW32_0.1 = phi i32 [ %t1760, %L_492 ], [ %TW32_0.2, %loop_17 ]
  %t1975 = tail call i64 @Posix_Error_strError(i32 %TW32_0.1)
  %t1969 = inttoptr i64 %t1975 to i8*
  %t1971.not = icmp eq i64 %t1975, 0
  br i1 %t1971.not, label %L_480, label %loop_18

loop_17:                                          ; preds = %L_464, %L_501
  %storemerge.in = phi i32 [ %t1700, %L_501 ], [ %t1990, %L_464 ]
  %TW32_0.2 = phi i32 [ %t1705, %L_501 ], [ %t1995, %L_464 ]
  %storemerge = add i32 %storemerge.in, -1
  store i32 %storemerge, i32* %t2079, align 4
  %t1977.not = icmp eq i32 %TW32_0.2, 4
  br i1 %t1977.not, label %L_482, label %errorMsg_0

L_464:                                            ; preds = %L_453
  %t1995 = tail call i32 @Posix_Error_getErrno()
  %t1990 = load i32, i32* %t2079, align 4
  %t1992.not = icmp eq i32 %t1990, 0
  br i1 %t1992.not, label %L_462, label %loop_17

L_462:                                            ; preds = %L_456, %L_464, %L_486, %L_497, %L_501, %L_490
  %t2018 = load i8*, i8** %t1852, align 8
  %cond25 = icmp eq i8* %t2018, inttoptr (i64 1 to i8*)
  br i1 %cond25, label %L_438, label %L_421.backedge

L_460:                                            ; preds = %L_457, %L_487, %L_498, %L_505, %L_507, %L_449
  %t2042 = load i8*, i8** %t1852, align 8
  %cond33 = icmp eq i8* %t2042, inttoptr (i64 1 to i8*)
  br i1 %cond33, label %L_438, label %L_421.backedge

L_457:                                            ; preds = %L_456
  %t2057 = add i32 %t2065, -1
  store i32 %t2057, i32* %t2079, align 4
  %cond32 = icmp eq i8* %TP_0.1, inttoptr (i64 1 to i8*)
  br i1 %cond32, label %L_460, label %L_451.backedge

L_451.backedge:                                   ; preds = %L_457, %L_487, %L_498, %L_505, %L_507
  br label %L_451

L_456:                                            ; preds = %L_453
  %t2065 = load i32, i32* %t2079, align 4
  %t2067.not = icmp eq i32 %t2065, 0
  br i1 %t2067.not, label %L_462, label %L_457

L_453:                                            ; preds = %L_452
  store i32 1, i32* %t2093472, align 4
  %t2080 = load i32, i32* %t2079, align 4
  %t2081 = add i32 %t2080, 1
  store i32 %t2081, i32* %t2079, align 4
  %t2085 = tail call i32 @Posix_IO_close(i32 0)
  %t2072.not = icmp eq i32 %t2085, -1
  br i1 %t2072.not, label %L_464, label %L_456

L_452:                                            ; preds = %L_451
  store i8 1, i8* %t2102, align 1
  %t2091 = getelementptr inbounds i8, i8* %TP_1.0, i64 8
  %23 = bitcast i8* %t2091 to i32**
  %t2093472 = load i32*, i32** %23, align 8
  %t2097 = load i32, i32* %t2093472, align 4
  %switch658 = icmp eq i32 %t2097, 0
  br i1 %switch658, label %L_453, label %L_505

L_451:                                            ; preds = %L_449, %L_451.backedge
  %TP_1.0.in.in = phi i8* [ %TP_0.1, %L_451.backedge ], [ %t2124, %L_449 ]
  %TP_0.1.in.in = getelementptr inbounds i8, i8* %TP_1.0.in.in, i64 8
  %TP_1.0.in = bitcast i8* %TP_1.0.in.in to i8**
  %TP_1.0 = load i8*, i8** %TP_1.0.in, align 8
  %TP_0.1.in = bitcast i8* %TP_0.1.in.in to i8**
  %TP_0.1 = load i8*, i8** %TP_0.1.in, align 8
  %t2101 = bitcast i8* %TP_1.0 to i8**
  %t2102 = load i8*, i8** %t2101, align 8
  %t2106 = load i8, i8* %t2102, align 1
  %switch659 = icmp eq i8 %t2106, 0
  br i1 %switch659, label %L_452, label %L_507

L_449:                                            ; preds = %L_422
  %t2124 = load i8*, i8** %t2490487, align 8
  %cond22 = icmp eq i8* %t2124, inttoptr (i64 1 to i8*)
  br i1 %cond22, label %L_460, label %L_451

L_425:                                            ; preds = %L_424
  %t2131 = bitcast i8* %t1943 to i64*
  store i64 163, i64* %t2131, align 4
  %t2133 = getelementptr inbounds i8, i8* %stackTop.24.ph729, i64 56
  store i8* %frontier.25.ph730, i8** %t5312, align 8
  store i8* %t2133, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t2143 = load i8*, i8** %t5312, align 8
  %t2146 = load i8*, i8** %t5315, align 8
  %t2128 = getelementptr inbounds i8, i8* %t2146, i64 -56
  %t2461.phi.trans.insert = getelementptr inbounds i8, i8* %t2146, i64 -16
  %t2462.phi.trans.insert = bitcast i8* %t2461.phi.trans.insert to i8**
  %t2463.pre = load i8*, i8** %t2462.phi.trans.insert, align 8
  br label %L_428

L_447:                                            ; preds = %doSwitchNextBlock
  %t2167 = getelementptr inbounds i8, i8* %stackTop.0, i64 -32
  %t2169 = getelementptr inbounds i8, i8* %stackTop.0, i64 -24
  %t2170 = bitcast i8* %t2169 to i8**
  %t2171 = load i8*, i8** %t2170, align 8
  %cond37 = icmp eq i8* %t2171, inttoptr (i64 1 to i8*)
  br i1 %cond37, label %L_438, label %L_421.outer.preheader

L_421.outer.preheader:                            ; preds = %L_419, %L_436, %L_440, %L_447
  %t2171.sink1273.sink.ph = phi i8* [ %t2528, %L_419 ], [ %t2352, %L_436 ], [ %t2280, %L_440 ], [ %t2171, %L_447 ]
  %t2170.sink.sink.ph = phi i8** [ %t3105, %L_419 ], [ %t2351, %L_436 ], [ %t2279, %L_440 ], [ %t2170, %L_447 ]
  %stackTop.24.ph729.ph = phi i8* [ %stackTop.29, %L_419 ], [ %stackTop.20, %L_436 ], [ %t2276, %L_440 ], [ %t2167, %L_447 ]
  %frontier.25.ph730.ph = phi i8* [ %frontier.30, %L_419 ], [ %frontier.21, %L_436 ], [ %frontier.0, %L_440 ], [ %frontier.0, %L_447 ]
  br label %L_421.outer

L_1826:                                           ; preds = %doSwitchNextBlock
  %t2197 = load i8*, i8** %t5411, align 8
  %t2199.not = icmp ult i8* %t2197, %frontier.0
  br i1 %t2199.not, label %L_434.sink.split, label %L_434

L_442:                                            ; preds = %L_428
  store i64 20, i64* %t2416, align 4
  %t2233 = getelementptr inbounds i8, i8* %stackTop.22, i64 32
  %t2237 = load i8*, i8** %t4075, align 8
  %t2238 = ptrtoint i8* %t2233 to i64
  %t2239 = ptrtoint i8* %t2237 to i64
  %t2240 = sub i64 %t2238, %t2239
  store i64 %t2240, i64* %t4081, align 4
  %t2244 = getelementptr inbounds i8, i8* %stackTop.22, i64 56
  %t2245 = bitcast i8* %t2244 to i8**
  store i8* %t2398, i8** %t2245, align 8
  %t2248 = getelementptr inbounds i8, i8* %stackTop.22, i64 64
  %t2249 = bitcast i8* %t2248 to i8**
  store i8* %t2448, i8** %t2249, align 8
  %t2252 = getelementptr inbounds i8, i8* %stackTop.22, i64 48
  %t2253 = bitcast i8* %t2252 to i64*
  store i64 21, i64* %t2253, align 4
  br label %L_345

L_440:                                            ; preds = %doSwitchNextBlock
  %t2276 = getelementptr inbounds i8, i8* %stackTop.0, i64 -32
  %t2278 = getelementptr inbounds i8, i8* %stackTop.0, i64 -24
  %t2279 = bitcast i8* %t2278 to i8**
  %t2280 = load i8*, i8** %t2279, align 8
  %cond36 = icmp eq i8* %t2280, inttoptr (i64 1 to i8*)
  br i1 %cond36, label %L_438, label %L_421.outer.preheader

L_438:                                            ; preds = %L_419, %L_447, %L_440, %L_436, %L_475, %x_2, %L_460, %L_478, %L_480, %L_462
  %stackTop.19 = phi i8* [ %stackTop.24.ph729, %L_462 ], [ %stackTop.24.ph729, %L_480 ], [ %stackTop.24.ph729, %L_478 ], [ %stackTop.24.ph729, %L_460 ], [ %stackTop.24.ph729, %x_2 ], [ %t1906, %L_475 ], [ %t2167, %L_447 ], [ %t2276, %L_440 ], [ %stackTop.20, %L_436 ], [ %stackTop.29, %L_419 ]
  %frontier.20 = phi i8* [ %frontier.25.ph730, %L_462 ], [ %frontier.25.ph730, %L_480 ], [ %frontier.25.ph730, %L_478 ], [ %frontier.25.ph730, %L_460 ], [ %frontier.25.ph730, %x_2 ], [ %t1934, %L_475 ], [ %frontier.0, %L_447 ], [ %frontier.0, %L_440 ], [ %frontier.21, %L_436 ], [ %frontier.30, %L_419 ]
  %t2310 = bitcast i8* %stackTop.19 to i32*
  %t2311 = load i32, i32* %t2310, align 4
  %t2313 = getelementptr inbounds i8, i8* %stackTop.19, i64 40
  %t2314 = bitcast i8* %t2313 to i64*
  store i64 162, i64* %t2314, align 4
  %t2316 = getelementptr inbounds i8, i8* %stackTop.19, i64 48
  store i8* %frontier.20, i8** %t5312, align 8
  store i8* %t2316, i8** %t5315, align 8
  tail call void @MLton_halt(i8* %gcState, i32 %t2311)
  tail call void @MLton_bug(i8* getelementptr (i8, i8* @staticHeapI, i64 3624))
  %t2307 = tail call i64 @MLton_unreachable()
  br label %common.ret

L_436:                                            ; preds = %L_434, %L_423
  %stackTop.20 = phi i8* [ %stackTop.24.ph729, %L_423 ], [ %stackTop.21, %L_434 ]
  %frontier.21 = phi i8* [ %frontier.25.ph730, %L_423 ], [ %frontier.22, %L_434 ]
  %t2350 = getelementptr inbounds i8, i8* %stackTop.20, i64 8
  %t2351 = bitcast i8* %t2350 to i8**
  %t2352 = load i8*, i8** %t2351, align 8
  %cond20 = icmp eq i8* %t2352, inttoptr (i64 1 to i8*)
  br i1 %cond20, label %L_438, label %L_421.outer.preheader

L_434.sink.split:                                 ; preds = %L_1825, %L_1826
  %t2285 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t2286 = bitcast i8* %t2285 to i64*
  store i64 163, i64* %t2286, align 4
  store i8* %frontier.0, i8** %t5312, align 8
  store i8* %stackTop.0, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t2298 = load i8*, i8** %t5312, align 8
  %t2301 = load i8*, i8** %t5315, align 8
  br label %L_434

L_434:                                            ; preds = %L_434.sink.split, %L_1825, %L_1826
  %stackTop.0.pn = phi i8* [ %stackTop.0, %L_1825 ], [ %stackTop.0, %L_1826 ], [ %t2301, %L_434.sink.split ]
  %frontier.22 = phi i8* [ %frontier.0, %L_1825 ], [ %frontier.0, %L_1826 ], [ %t2298, %L_434.sink.split ]
  %stackTop.21 = getelementptr inbounds i8, i8* %stackTop.0.pn, i64 -56
  %t2374 = getelementptr inbounds i8, i8* %stackTop.0.pn, i64 -16
  %t2375 = bitcast i8* %t2374 to i8**
  %t2376 = load i8*, i8** %t2375, align 8
  %cond19 = icmp eq i8* %t2376, inttoptr (i64 1 to i8*)
  br i1 %cond19, label %L_436, label %L_428

L_1825:                                           ; preds = %doSwitchNextBlock
  %t2382 = load i8*, i8** %t5411, align 8
  %t2384.not = icmp ult i8* %t2382, %frontier.0
  br i1 %t2384.not, label %L_434.sink.split, label %L_434

L_429:                                            ; preds = %L_428
  store i64 22, i64* %t2416, align 4
  %t2418 = getelementptr inbounds i8, i8* %stackTop.22, i64 32
  %t2422 = load i8*, i8** %t4075, align 8
  %t2423 = ptrtoint i8* %t2418 to i64
  %t2424 = ptrtoint i8* %t2422 to i64
  %t2425 = sub i64 %t2423, %t2424
  store i64 %t2425, i64* %t4081, align 4
  %t2429 = getelementptr inbounds i8, i8* %stackTop.22, i64 56
  %t2430 = bitcast i8* %t2429 to i8**
  store i8* %t2398, i8** %t2430, align 8
  %t2433 = getelementptr inbounds i8, i8* %stackTop.22, i64 64
  %t2434 = bitcast i8* %t2433 to i8**
  store i8* %t2448, i8** %t2434, align 8
  %t2437 = getelementptr inbounds i8, i8* %stackTop.22, i64 48
  %t2438 = bitcast i8* %t2437 to i64*
  store i64 23, i64* %t2438, align 4
  br label %L_345

L_428:                                            ; preds = %L_434, %L_425, %L_424
  %t2463.sink1272 = phi i8* [ %t2497, %L_424 ], [ %t2463.pre, %L_425 ], [ %t2376, %L_434 ]
  %t2462.pre-phi.sink = phi i8** [ %t2493, %L_424 ], [ %t2462.phi.trans.insert, %L_425 ], [ %t2375, %L_434 ]
  %stackTop.22 = phi i8* [ %stackTop.24.ph729, %L_424 ], [ %t2128, %L_425 ], [ %stackTop.21, %L_434 ]
  %frontier.23 = phi i8* [ %frontier.25.ph730, %L_424 ], [ %t2143, %L_425 ], [ %frontier.22, %L_434 ]
  %t2464 = getelementptr inbounds i8, i8* %t2463.sink1272, i64 8
  %t2465 = bitcast i8* %t2464 to i8**
  %t2466 = load i8*, i8** %t2465, align 8
  %t2472 = bitcast i8* %t2463.sink1272 to i8**
  %t2473 = load i8*, i8** %t2472, align 8
  store i8* %t2466, i8** %t2462.pre-phi.sink, align 8
  %t2442 = getelementptr inbounds i8, i8* %t2473, i64 16
  %24 = bitcast i8* %t2442 to i8***
  %t2444489 = load i8**, i8*** %24, align 8
  %t2446 = getelementptr inbounds i8, i8* %t2473, i64 24
  %t2447 = bitcast i8* %t2446 to i8**
  %t2448 = load i8*, i8** %t2447, align 8
  %t2452 = load i8*, i8** %t2444489, align 8
  %t2454 = getelementptr inbounds i8, i8* %t2452, i64 -8
  %t2455 = bitcast i8* %t2454 to i64*
  %t2456 = load i64, i64* %t2455, align 4
  %t2458.mask = and i64 %t2456, -2
  %switch662 = icmp eq i64 %t2458.mask, 118
  %t2389 = getelementptr inbounds i8, i8* %t2452, i64 8
  %t2390 = bitcast i8* %t2389 to i8**
  %t2391 = load i8*, i8** %t2390, align 8
  %t2395 = bitcast i8* %t2452 to i8**
  %t2396 = load i8*, i8** %t2395, align 8
  %t2398 = getelementptr inbounds i8, i8* %frontier.23, i64 8
  %t2403 = bitcast i8* %frontier.23 to i64*
  store i64 79, i64* %t2403, align 4
  %t2405 = getelementptr inbounds i8, i8* %frontier.23, i64 24
  %t2408 = bitcast i8* %t2398 to i8**
  store i8* %t2391, i8** %t2408, align 8
  %t2411 = getelementptr inbounds i8, i8* %frontier.23, i64 16
  %t2412 = bitcast i8* %t2411 to i8**
  store i8* %t2396, i8** %t2412, align 8
  %t2415 = getelementptr inbounds i8, i8* %stackTop.22, i64 24
  %t2416 = bitcast i8* %t2415 to i64*
  br i1 %switch662, label %L_429, label %L_442

L_424:                                            ; preds = %L_423
  %t2481 = load i8*, i8** %t5411, align 8
  %t2483.not = icmp ult i8* %t2481, %frontier.25.ph730
  br i1 %t2483.not, label %L_425, label %L_428

L_423:                                            ; preds = %L_422
  %t2493 = bitcast i8* %t1939 to i8**
  %t2497 = load i8*, i8** %t2490487, align 8
  store i8* %t2497, i8** %t2493, align 8
  %cond34 = icmp eq i8* %t2497, inttoptr (i64 1 to i8*)
  br i1 %cond34, label %L_436, label %L_424

L_422:                                            ; preds = %L_421
  %t2504 = getelementptr inbounds i8, i8* %TP_0.3, i64 -8
  %t2505 = bitcast i8* %t2504 to i64*
  %t2506 = load i64, i64* %t2505, align 4
  %t2508.mask = and i64 %t2506, -2
  %switch664 = icmp eq i64 %t2508.mask, 102
  %25 = bitcast i8* %TP_0.3 to i8***
  %t2490487 = load i8**, i8*** %25, align 8
  br i1 %switch664, label %L_423, label %L_449

L_421:                                            ; preds = %L_421.backedge, %L_421.outer
  %TP_0.3 = phi i8* [ %t2160, %L_421.outer ], [ %t1655, %L_421.backedge ]
  %cond21 = icmp eq i8* %TP_0.3, inttoptr (i64 1 to i8*)
  br i1 %cond21, label %x_2, label %L_422

L_419:                                            ; preds = %L_372
  %t2528 = load i8*, i8** %t3116450, align 8
  %cond38 = icmp eq i8* %t2528, inttoptr (i64 1 to i8*)
  br i1 %cond38, label %L_438, label %L_421.outer.preheader

L_418:                                            ; preds = %L_410
  %t2533 = load i8*, i8** %t4075, align 8
  %t2536 = load i64, i64* %t4081, align 4
  %t2537 = getelementptr inbounds i8, i8* %t2533, i64 %t2536
  %t2539 = getelementptr inbounds i8, i8* %t2537, i64 -8
  %t2540 = bitcast i8* %t2539 to i64*
  %t2541 = load i64, i64* %t2540, align 4
  store i8* %frontier.30, i8** %t5312, align 8
  store i8* %t2537, i8** %t5315, align 8
  br label %common.ret

L_416:                                            ; preds = %L_411
  %t2562 = load i32, i32* %.pre1097, align 4
  br label %loop_14.preheader

L_411:                                            ; preds = %L_410
  %t2577 = add i32 %t2601, -1
  store i32 %t2577, i32* %t2079, align 4
  %t2582 = getelementptr inbounds i8, i8* %stackTop.29, i64 48
  %t2583 = bitcast i8* %t2582 to i64*
  store i64 161, i64* %t2583, align 4
  %t2585 = getelementptr inbounds i8, i8* %stackTop.29, i64 56
  store i8* %frontier.30, i8** %t5312, align 8
  store i8* %t2585, i8** %t5315, align 8
  %t2592 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 33, i64 21)
  %t2595 = load i8*, i8** %t5312, align 8
  %t2598 = load i8*, i8** %t5315, align 8
  call void @llvm.memset.p0i8.i64(i8* noundef nonnull align 1 dereferenceable(33) %t2592, i8 0, i64 33, i1 false)
  %t2572 = getelementptr inbounds i8, i8* %t2598, i64 -56
  %t2564 = getelementptr inbounds i8, i8* %t2598, i64 -52
  %t2565 = bitcast i8* %t2564 to i32*
  %t2566 = load i32, i32* %t2565, align 4
  %switch665 = icmp eq i32 %t2566, 0
  %.pre1097 = bitcast i8* %t2572 to i32*
  br i1 %switch665, label %L_380, label %L_416

L_410:                                            ; preds = %L_374
  %t2601 = load i32, i32* %t2079, align 4
  %t2603.not = icmp eq i32 %t2601, 0
  br i1 %t2603.not, label %L_418, label %L_411

L_409:                                            ; preds = %L_376
  %t2608 = load i8*, i8** %t4075, align 8
  %t2611 = load i64, i64* %t4081, align 4
  %t2612 = getelementptr inbounds i8, i8* %t2608, i64 %t2611
  %t2614 = getelementptr inbounds i8, i8* %t2612, i64 -8
  %t2615 = bitcast i8* %t2614 to i64*
  %t2616 = load i64, i64* %t2615, align 4
  store i8* %frontier.30, i8** %t5312, align 8
  store i8* %t2612, i8** %t5315, align 8
  br label %common.ret

L_408:                                            ; preds = %L_377
  %t2628 = load i32, i32* %t3092, align 4
  br label %loop_14.preheader

L_407:                                            ; preds = %L_406
  %t2635 = getelementptr inbounds i8, i8* %TP_0.5.ph, i64 %t2640
  store i8 126, i8* %t2635, align 1
  br label %L_390

L_406:                                            ; preds = %L_405
  %t2647 = add nsw i32 %TW32_0.4877, -1
  %t2640 = sext i32 %t2647 to i64
  %t2643.not = icmp ugt i64 %t2952, %t2640
  br i1 %t2643.not, label %L_407, label %L_381

L_405:                                            ; preds = %L_1822
  %t2650.not = icmp eq i32 %TW32_0.4877, -2147483648
  br i1 %t2650.not, label %L_381, label %L_406

L_404:                                            ; preds = %L_403, %L_404
  %TW64_2.0893 = phi i64 [ %t2678, %L_404 ], [ 0, %L_403 ]
  %t2662 = add i64 %TW64_2.0893, %t2694
  %t2666 = getelementptr inbounds i8, i8* %t2698, i64 %t2662
  %t2669 = load i8, i8* %t2666, align 1
  %t2673 = getelementptr inbounds i8, i8* %t2875, i64 %TW64_2.0893
  store i8 %t2669, i8* %t2673, align 1
  %t2678 = add i64 %TW64_2.0893, 1
  %t2682.not = icmp slt i64 %t2689, %t2678
  br i1 %t2682.not, label %L_395, label %L_404

L_403:                                            ; preds = %L_join_4
  %t2689 = add i64 %t2853.pre, -1
  %t2693 = bitcast i8* %t2858 to i64*
  %t2694 = load i64, i64* %t2693, align 4
  %t2696 = getelementptr inbounds i8, i8* %t2881, i64 -24
  %t2697 = bitcast i8* %t2696 to i8**
  %t2698 = load i8*, i8** %t2697, align 8
  %t2682.not892 = icmp slt i64 %t2689, 0
  br i1 %t2682.not892, label %L_395, label %L_404

L_396:                                            ; preds = %L_395
  %t2702 = getelementptr inbounds i8, i8* %t2858.sink, i64 48
  %t2703 = bitcast i8* %t2702 to i64*
  store i64 160, i64* %t2703, align 4
  store i8* %frontier.271110, i8** %t5312, align 8
  store i8* %t28511112, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t2715 = load i8*, i8** %t5312, align 8
  %t2718 = load i8*, i8** %t5315, align 8
  %t2700 = getelementptr inbounds i8, i8* %t2718, i64 -56
  %t2817.phi.trans.insert = bitcast i8* %t2700 to i8**
  %t2818.pre = load i8*, i8** %t2817.phi.trans.insert, align 8
  br label %L_398

L_1823:                                           ; preds = %doSwitchNextBlock
  %t2724 = getelementptr inbounds i8, i8* %stackTop.0, i64 -32
  %t2725 = bitcast i8* %t2724 to i64*
  %t2726 = load i64, i64* %t2725, align 4
  store i64 %t2726, i64* %t4081, align 4
  %t2729 = load i8*, i8** %t4075, align 8
  %t2733 = getelementptr inbounds i8, i8* %t2729, i64 %t2726
  %t2735 = getelementptr inbounds i8, i8* %t2733, i64 -8
  %t2736 = bitcast i8* %t2735 to i64*
  %t2737 = load i64, i64* %t2736, align 4
  store i8* %frontier.0, i8** %t5312, align 8
  store i8* %t2733, i8** %t5315, align 8
  br label %common.ret

L_400:                                            ; preds = %L_398, %L_399
  %t2747 = getelementptr inbounds i8, i8* %frontier.26, i64 8
  %t2752 = bitcast i8* %frontier.26 to i64*
  store i64 123, i64* %t2752, align 4
  %t2757 = bitcast i8* %t2747 to i8**
  %t2761 = load i8*, i8** %t2817.pre-phi, align 8
  store i8* %t2761, i8** %t2757, align 8
  %t2764 = getelementptr inbounds i8, i8* %frontier.26, i64 16
  %t2765 = bitcast i8* %t2764 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 9120), i8** %t2765, align 8
  %t2769 = getelementptr inbounds i8, i8* %frontier.26, i64 32
  %t2773 = getelementptr inbounds i8, i8* %frontier.26, i64 24
  %t2774 = bitcast i8* %t2773 to i64*
  store i64 123, i64* %t2774, align 4
  %t2776 = getelementptr inbounds i8, i8* %frontier.26, i64 48
  %t2779 = bitcast i8* %t2769 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4656), i8** %t2779, align 8
  %t2783 = getelementptr inbounds i8, i8* %frontier.26, i64 40
  %t2784 = bitcast i8* %t2783 to i8**
  store i8* %t2747, i8** %t2784, align 8
  %t2787 = getelementptr inbounds i8, i8* %stackTop.25, i64 24
  %t2788 = bitcast i8* %t2787 to i64*
  store i64 1, i64* %t2788, align 4
  %t2790 = getelementptr inbounds i8, i8* %stackTop.25, i64 32
  %t2794 = load i8*, i8** %t4075, align 8
  %t2795 = ptrtoint i8* %t2790 to i64
  %t2796 = ptrtoint i8* %t2794 to i64
  %t2797 = sub i64 %t2795, %t2796
  store i64 %t2797, i64* %t4081, align 4
  %t2801 = getelementptr inbounds i8, i8* %stackTop.25, i64 48
  %t2802 = bitcast i8* %t2801 to i8**
  store i8* %t2769, i8** %t2802, align 8
  br label %L_511.sink.split

L_399:                                            ; preds = %L_398
  %t2810 = getelementptr inbounds i8, i8* %stackTop.25, i64 8
  %26 = bitcast i8* %t2810 to i32**
  %t2812465 = load i32*, i32** %26, align 8
  store i32 0, i32* %t2812465, align 4
  br label %L_400

L_398:                                            ; preds = %L_395.L_398_crit_edge, %L_396
  %t2817.pre-phi = phi i8** [ %.pre1098, %L_395.L_398_crit_edge ], [ %t2817.phi.trans.insert, %L_396 ]
  %t2818 = phi i8* [ %t2875.sink, %L_395.L_398_crit_edge ], [ %t2818.pre, %L_396 ]
  %stackTop.25 = phi i8* [ %t2858.sink, %L_395.L_398_crit_edge ], [ %t2700, %L_396 ]
  %frontier.26 = phi i8* [ %frontier.271110, %L_395.L_398_crit_edge ], [ %t2715, %L_396 ]
  %t2819 = getelementptr inbounds i8, i8* %t2818, i64 -8
  %t2820 = bitcast i8* %t2819 to i64*
  store i64 11, i64* %t2820, align 4
  %t2822 = getelementptr inbounds i8, i8* %stackTop.25, i64 40
  %t2823 = bitcast i8* %t2822 to i32*
  %t2824 = load i32, i32* %t2823, align 4
  %switch671 = icmp eq i32 %t2824, 0
  br i1 %switch671, label %L_399, label %L_400

L_395:                                            ; preds = %L_404, %L_403, %L_403.thread, %L_393
  %t2858.sink = phi i8* [ %t2858, %L_393 ], [ %stackTop.27.ph, %L_403.thread ], [ %t2858, %L_403 ], [ %t2858, %L_404 ]
  %t2875.sink = phi i8* [ %t2875, %L_393 ], [ getelementptr (i8, i8* @staticHeapM, i64 64), %L_403.thread ], [ %t2875, %L_403 ], [ %t2875, %L_404 ]
  %t28511112 = phi i8* [ %t2851, %L_393 ], [ %t28511103, %L_403.thread ], [ %t2851, %L_403 ], [ %t2851, %L_404 ]
  %frontier.271110 = phi i8* [ %t2878, %L_393 ], [ %frontier.28.ph, %L_403.thread ], [ %t2878, %L_403 ], [ %t2878, %L_404 ]
  %t2834 = bitcast i8* %t2858.sink to i8**
  store i8* %t2875.sink, i8** %t2834, align 8
  %t2827 = load i8*, i8** %t5411, align 8
  %t2829.not = icmp ult i8* %t2827, %frontier.271110
  br i1 %t2829.not, label %L_396, label %L_395.L_398_crit_edge

L_395.L_398_crit_edge:                            ; preds = %L_395
  %.pre1098 = bitcast i8* %t2858.sink to i8**
  br label %L_398

L_393:                                            ; preds = %L_join_4
  %t2838 = getelementptr inbounds i8, i8* %t2881, i64 -24
  %t2839 = bitcast i8* %t2838 to i8**
  %t2840 = load i8*, i8** %t2839, align 8
  %t2843 = bitcast i8* %t2858 to i64*
  %t2844 = load i64, i64* %t2843, align 4
  tail call void @GC_sequenceCopy(i8* nonnull %gcState, i8* %t2875, i64 0, i8* %t2840, i64 %t2844, i64 %t2853.pre)
  br label %L_395

L_join_4:                                         ; preds = %L_391
  %t2865 = getelementptr inbounds i8, i8* %stackTop.27.ph, i64 64
  %t2866 = bitcast i8* %t2865 to i64*
  store i64 159, i64* %t2866, align 4
  %t2868 = getelementptr inbounds i8, i8* %stackTop.27.ph, i64 72
  store i8* %frontier.28.ph, i8** %t5312, align 8
  store i8* %t2868, i8** %t5315, align 8
  %t2875 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t2887, i64 21)
  %t2878 = load i8*, i8** %t5312, align 8
  %t2881 = load i8*, i8** %t5315, align 8
  %t2858 = getelementptr inbounds i8, i8* %t2881, i64 -72
  %t2851.phi.trans.insert = getelementptr inbounds i8, i8* %t2881, i64 -16
  %t2852.phi.trans.insert = bitcast i8* %t2851.phi.trans.insert to i64*
  %t2853.pre = load i64, i64* %t2852.phi.trans.insert, align 4
  %t2851 = getelementptr inbounds i8, i8* %t2881, i64 -16
  %t2854 = icmp sgt i64 %t2853.pre, 4
  br i1 %t2854, label %L_393, label %L_403

L_391:                                            ; preds = %L_390
  %t2887 = sub i64 %t2952, %t2899.pre-phi
  %t2889 = getelementptr inbounds i8, i8* %stackTop.27.ph, i64 56
  %t2890 = bitcast i8* %t2889 to i64*
  store i64 %t2887, i64* %t2890, align 4
  %t2895.not = icmp eq i64 %t2887, 0
  br i1 %t2895.not, label %L_403.thread, label %L_join_4

L_403.thread:                                     ; preds = %L_391
  %t28511103 = getelementptr inbounds i8, i8* %stackTop.27.ph, i64 56
  br label %L_395

L_390:                                            ; preds = %L_1822, %L_407
  %t2899.pre-phi = phi i64 [ %t2954, %L_1822 ], [ %t2640, %L_407 ]
  %t2902 = bitcast i8* %stackTop.27.ph to i64*
  store i64 %t2899.pre-phi, i64* %t2902, align 4
  %t2908.not = icmp ult i64 %t2952, %t2899.pre-phi
  br i1 %t2908.not, label %L_381, label %L_391

L_1822:                                           ; preds = %L_385
  %t2912 = getelementptr inbounds i8, i8* %stackTop.27.ph, i64 48
  %t2913 = bitcast i8* %t2912 to i8**
  store i8* %TP_0.5.ph, i8** %t2913, align 8
  %t2916 = getelementptr inbounds i8, i8* %stackTop.27.ph, i64 4
  %t2917 = bitcast i8* %t2916 to i32*
  %t2918 = load i32, i32* %t2917, align 4
  %switch676 = icmp eq i32 %t2918, 0
  br i1 %switch676, label %L_390, label %L_405

L_387:                                            ; preds = %L_386
  %t2922 = add nsw i32 %TW32_0.4877, -1
  %t3009 = srem i32 %t2936, 10
  %t3011 = sub nsw i32 0, %t3009
  %t3015 = icmp ugt i32 %t3011, 15
  br i1 %t3015, label %L_381, label %L_384

L_386:                                            ; preds = %L_385
  %t2925.not = icmp eq i32 %TW32_0.4877, -2147483648
  br i1 %t2925.not, label %L_381, label %L_387

L_385:                                            ; preds = %L_384
  %t30131143 = zext i32 %t3011879 to i64
  %t2945 = getelementptr inbounds i8, i8* getelementptr (i8, i8* @staticHeapI, i64 4696), i64 %t30131143
  %t2948 = load i8, i8* %t2945, align 1
  %t2931 = getelementptr inbounds i8, i8* %TP_0.5.ph, i64 %t2954
  store i8 %t2948, i8* %t2931, align 1
  %t2936 = sdiv i32 %TW32_1.0878, 10
  %27 = add i32 %TW32_1.0878, -10
  %28 = icmp ult i32 %27, -19
  br i1 %28, label %L_386, label %L_1822

L_384:                                            ; preds = %L_384.lr.ph, %L_387
  %t3011879 = phi i32 [ %t3011875, %L_384.lr.ph ], [ %t3011, %L_387 ]
  %TW32_1.0878 = phi i32 [ %TW32_1.0.ph, %L_384.lr.ph ], [ %t2936, %L_387 ]
  %TW32_0.4877 = phi i32 [ 32, %L_384.lr.ph ], [ %t2922, %L_387 ]
  %t2952 = load i64, i64* %t2951, align 4
  %t2954 = sext i32 %TW32_0.4877 to i64
  %t2957.not = icmp ugt i64 %t2952, %t2954
  br i1 %t2957.not, label %L_385, label %L_381

L_383:                                            ; preds = %L_381
  %t2962 = load i8*, i8** %t4075, align 8
  %t2965 = load i64, i64* %t4081, align 4
  %t2966 = getelementptr inbounds i8, i8* %t2962, i64 %t2965
  %t2968 = getelementptr inbounds i8, i8* %t2966, i64 -8
  %t2969 = bitcast i8* %t2968 to i64*
  %t2970 = load i64, i64* %t2969, align 4
  store i8* %frontier.28.ph, i8** %t5312, align 8
  store i8* %t2966, i8** %t5315, align 8
  br label %common.ret

L_382:                                            ; preds = %L_381
  %t2980 = getelementptr inbounds i8, i8* %stackTop.27.ph, i64 8
  %29 = bitcast i8* %t2980 to i32**
  %t2982458 = load i32*, i32** %29, align 8
  store i32 0, i32* %t2982458, align 4
  %t2987 = load i8*, i8** %t4075, align 8
  %t2990 = load i64, i64* %t4081, align 4
  %t2991 = getelementptr inbounds i8, i8* %t2987, i64 %t2990
  %t2993 = getelementptr inbounds i8, i8* %t2991, i64 -8
  %t2994 = bitcast i8* %t2993 to i64*
  %t2995 = load i64, i64* %t2994, align 4
  store i8* %frontier.28.ph, i8** %t5312, align 8
  store i8* %t2991, i8** %t5315, align 8
  br label %common.ret

L_381:                                            ; preds = %L_390, %L_405, %L_406, %loop_14.preheader, %L_387, %L_384, %L_386
  %t3005 = getelementptr inbounds i8, i8* %stackTop.27.ph, i64 40
  %t3006 = bitcast i8* %t3005 to i32*
  %t3007 = load i32, i32* %t3006, align 4
  %switch680 = icmp eq i32 %t3007, 0
  br i1 %switch680, label %L_382, label %L_383

L_380:                                            ; preds = %L_411, %L_377
  %t3020.pre-phi = phi i32* [ %t3092, %L_377 ], [ %.pre1097, %L_411 ]
  %TP_0.6 = phi i8* [ %t3051, %L_377 ], [ %t2592, %L_411 ]
  %stackTop.28 = phi i8* [ %stackTop.29, %L_377 ], [ %t2572, %L_411 ]
  %frontier.29 = phi i8* [ %frontier.30, %L_377 ], [ %t2595, %L_411 ]
  %t3021 = load i32, i32* %t3020.pre-phi, align 4
  %t3022 = sub i32 0, %t3021
  br label %loop_14.preheader

loop_14.preheader:                                ; preds = %L_416, %L_408, %L_380
  %TP_0.5.ph = phi i8* [ %t3051, %L_408 ], [ %TP_0.6, %L_380 ], [ %t2592, %L_416 ]
  %TW32_1.0.ph = phi i32 [ %t2628, %L_408 ], [ %t3022, %L_380 ], [ %t2562, %L_416 ]
  %stackTop.27.ph = phi i8* [ %stackTop.29, %L_408 ], [ %stackTop.28, %L_380 ], [ %t2572, %L_416 ]
  %frontier.28.ph = phi i8* [ %frontier.30, %L_408 ], [ %frontier.29, %L_380 ], [ %t2595, %L_416 ]
  %t3009874 = srem i32 %TW32_1.0.ph, 10
  %t3011875 = sub nsw i32 0, %t3009874
  %t3015876 = icmp ugt i32 %t3011875, 15
  br i1 %t3015876, label %L_381, label %L_384.lr.ph

L_384.lr.ph:                                      ; preds = %loop_14.preheader
  %t2950 = getelementptr inbounds i8, i8* %TP_0.5.ph, i64 -16
  %t2951 = bitcast i8* %t2950 to i64*
  br label %L_384

L_377:                                            ; preds = %L_376
  %t3027 = add i32 %t3043, -1
  store i32 %t3027, i32* %t2079, align 4
  %t3034 = load i32, i32* %t3098, align 4
  %switch682 = icmp eq i32 %t3034, 0
  br i1 %switch682, label %L_380, label %L_408

L_376:                                            ; preds = %L_374
  store i32 1, i32* %t3072451, align 4
  %t3043 = load i32, i32* %t2079, align 4
  %t3045.not = icmp eq i32 %t3043, 0
  br i1 %t3045.not, label %L_409, label %L_377

L_374:                                            ; preds = %L_372
  %t3049 = getelementptr inbounds i8, i8* %t3123, i64 8
  %t3050 = bitcast i8* %t3049 to i8**
  %t3051 = load i8*, i8** %t3050, align 8
  %t3057 = bitcast i8* %t3123 to i8**
  %t3058 = load i8*, i8** %t3057, align 8
  store i8* %t3058, i8** %t3105, align 8
  %t3061 = load i32, i32* %t2079, align 4
  %t3062 = add i32 %t3061, 1
  store i32 %t3062, i32* %t2079, align 4
  %t3067 = getelementptr inbounds i8, i8* %stackTop.29, i64 40
  %t3068 = bitcast i8* %t3067 to i32*
  %30 = bitcast i8* %t3104 to i32**
  %t3072451 = load i32*, i32** %30, align 8
  %t3075 = load i32, i32* %t3072451, align 4
  store i32 %t3075, i32* %t3068, align 4
  %switch684 = icmp eq i32 %t3075, 0
  br i1 %switch684, label %L_376, label %L_410

L_372:                                            ; preds = %L_371
  store i32 1, i32* %t3109449, align 4
  %t3092 = bitcast i8* %stackTop.29 to i32*
  %t3093 = load i32, i32* %t3092, align 4
  %t3093.lobit = lshr i32 %t3093, 31
  %t3097 = getelementptr inbounds i8, i8* %stackTop.29, i64 4
  %t3098 = bitcast i8* %t3097 to i32*
  store i32 %t3093.lobit, i32* %t3098, align 4
  %31 = icmp ugt i32 %t3093, 255
  br i1 %31, label %L_374, label %L_419

L_371:                                            ; preds = %L_1821, %L_369
  %stackTop.29 = phi i8* [ %t1604, %L_369 ], [ %stackTop.30, %L_1821 ]
  %frontier.30 = phi i8* [ %t1619, %L_369 ], [ %frontier.0, %L_1821 ]
  %t3104 = getelementptr inbounds i8, i8* %stackTop.29, i64 8
  %t3105 = bitcast i8* %t3104 to i8**
  %t3106 = load i8*, i8** %t3105, align 8
  %t3107 = getelementptr inbounds i8, i8* %t3106, i64 16
  %32 = bitcast i8* %t3107 to i32**
  %t3109449 = load i32*, i32** %32, align 8
  %t3114 = getelementptr inbounds i8, i8* %t3106, i64 8
  %33 = bitcast i8* %t3114 to i8***
  %t3116450 = load i8**, i8*** %33, align 8
  %t3122 = bitcast i8* %t3106 to i8**
  %t3123 = load i8*, i8** %t3122, align 8
  %t3127 = load i32, i32* %t3109449, align 4
  %switch687 = icmp eq i32 %t3127, 0
  br i1 %switch687, label %L_372, label %L_510

L_1821.sink.split:                                ; preds = %print_5, %L_1261
  %t5195.sink = phi i8* [ %t5195, %L_1261 ], [ %t5334, %print_5 ]
  %.sink = phi i64 [ 7, %L_1261 ], [ 10, %print_5 ]
  %stackTop.30.ph = phi i8* [ %t5185, %L_1261 ], [ %t5328, %print_5 ]
  %t5196 = bitcast i8* %t5195.sink to i64*
  store i64 %.sink, i64* %t5196, align 4
  br label %L_1821

L_1821:                                           ; preds = %L_1821.sink.split, %doSwitchNextBlock
  %stackTop.30 = phi i8* [ %stackTop.0, %doSwitchNextBlock ], [ %stackTop.30.ph, %L_1821.sink.split ]
  %t3130 = getelementptr inbounds i8, i8* %stackTop.30, i64 16
  %t3131 = bitcast i8* %t3130 to i64*
  %t3134 = load i64, i64* %t4081, align 4
  store i64 %t3134, i64* %t3131, align 4
  %t3137 = load i8*, i8** %t4312, align 8
  %t3139.not = icmp ult i8* %t3137, %stackTop.30
  br i1 %t3139.not, label %L_369, label %L_371

L_512:                                            ; preds = %L_511
  %t3145 = getelementptr inbounds i8, i8* %stackTop.33, i64 8
  %t3146 = bitcast i8* %t3145 to i64*
  store i64 158, i64* %t3146, align 4
  %t3148 = getelementptr inbounds i8, i8* %stackTop.33, i64 16
  store i8* %frontier.33, i8** %t5312, align 8
  store i8* %t3148, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t3158 = load i8*, i8** %t5312, align 8
  %t3161 = load i8*, i8** %t5315, align 8
  %t3143 = getelementptr inbounds i8, i8* %t3161, i64 -16
  br label %L_514

L_533:                                            ; preds = %L_514
  store i8* getelementptr (i8, i8* @staticHeapI, i64 3816), i8** %t3417, align 8
  %t3168 = getelementptr inbounds i8, i8* %stackTop.32, i64 -8
  %t3169 = bitcast i8* %t3168 to i64*
  %t3170 = load i64, i64* %t3169, align 4
  %t3171 = getelementptr inbounds [84 x i64 (i8*, i8*, i8*, i64)*], [84 x i64 (i8*, i8*, i8*, i64)*]* @nextChunks, i64 0, i64 %t3170
  %t3172 = load i64 (i8*, i8*, i8*, i64)*, i64 (i8*, i8*, i8*, i64)** %t3171, align 8
  %t3173 = icmp eq i64 (i8*, i8*, i8*, i64)* %t3172, @Chunk_11
  br i1 %t3173, label %doSwitchNextBlock.backedge, label %L_1937

L_1937:                                           ; preds = %L_533
  store i8* %frontier.32, i8** %t5312, align 8
  store i8* %stackTop.32, i8** %t5315, align 8
  br label %common.ret

L_532:                                            ; preds = %L_515
  store i8* %t3409, i8** %t3417, align 8
  %t3188 = getelementptr inbounds i8, i8* %stackTop.32, i64 -8
  %t3189 = bitcast i8* %t3188 to i64*
  %t3190 = load i64, i64* %t3189, align 4
  %t3191 = getelementptr inbounds [84 x i64 (i8*, i8*, i8*, i64)*], [84 x i64 (i8*, i8*, i8*, i64)*]* @nextChunks, i64 0, i64 %t3190
  %t3192 = load i64 (i8*, i8*, i8*, i64)*, i64 (i8*, i8*, i8*, i64)** %t3191, align 8
  %t3193 = icmp eq i64 (i8*, i8*, i8*, i64)* %t3192, @Chunk_11
  br i1 %t3193, label %doSwitchNextBlock.backedge, label %L_1939

L_1939:                                           ; preds = %L_532
  store i8* %frontier.32, i8** %t5312, align 8
  store i8* %stackTop.32, i8** %t5315, align 8
  br label %common.ret

L_531:                                            ; preds = %L_520, %L_519
  %t3202 = load i8*, i8** %t4075, align 8
  %t3205 = load i64, i64* %t4081, align 4
  %t3206 = getelementptr inbounds i8, i8* %t3202, i64 %t3205
  %t3209 = bitcast i8* %t3206 to i8**
  store i8* inttoptr (i64 2 to i8*), i8** %t3209, align 8
  %t3213 = load i8*, i8** %t4075, align 8
  %t3216 = load i64, i64* %t4081, align 4
  %t3217 = getelementptr inbounds i8, i8* %t3213, i64 %t3216
  %t3219 = getelementptr inbounds i8, i8* %t3217, i64 -8
  %t3220 = bitcast i8* %t3219 to i64*
  %t3221 = load i64, i64* %t3220, align 4
  store i8* %frontier.32, i8** %t5312, align 8
  store i8* %t3217, i8** %t5315, align 8
  br label %common.ret

L_530:                                            ; preds = %L_529, %L_530
  %TW64_3.0905 = phi i64 [ %t3258, %L_530 ], [ 0, %L_529 ]
  %t3242 = add i64 %TW64_3.0905, %TW64_0.3
  %t3246 = getelementptr inbounds i8, i8* %TP_1.1, i64 %TW64_3.0905
  %t3249 = load i8, i8* %t3246, align 1
  %t3253 = getelementptr inbounds i8, i8* %TP_2.0.ph, i64 %t3242
  store i8 %t3249, i8* %t3253, align 1
  %t3258 = add i64 %TW64_3.0905, 1
  %t3262.not = icmp slt i64 %t3266, %t3258
  br i1 %t3262.not, label %L_526, label %L_530

L_529:                                            ; preds = %L_523
  %t3266 = add i64 %t3312, -1
  %t3262.not904 = icmp slt i64 %t3266, 0
  br i1 %t3262.not904, label %L_526, label %L_530

L_528:                                            ; preds = %L_526
  %t3268 = getelementptr inbounds i8, i8* %TP_2.0.ph, i64 -8
  %t3269 = bitcast i8* %t3268 to i64*
  store i64 11, i64* %t3269, align 4
  %t3272 = bitcast i8* %stackTop.31.ph to i8**
  store i8* %TP_2.0.ph, i8** %t3272, align 8
  %t3276 = getelementptr inbounds i8, i8* %stackTop.31.ph, i64 -8
  %t3277 = bitcast i8* %t3276 to i64*
  %t3278 = load i64, i64* %t3277, align 4
  %t3279 = getelementptr inbounds [84 x i64 (i8*, i8*, i8*, i64)*], [84 x i64 (i8*, i8*, i8*, i64)*]* @nextChunks, i64 0, i64 %t3278
  %t3280 = load i64 (i8*, i8*, i8*, i64)*, i64 (i8*, i8*, i8*, i64)** %t3279, align 8
  %t3281 = icmp eq i64 (i8*, i8*, i8*, i64)* %t3280, @Chunk_11
  br i1 %t3281, label %doSwitchNextBlock.backedge, label %L_1942

L_1942:                                           ; preds = %L_528
  store i8* %frontier.31.ph, i8** %t5312, align 8
  store i8* %stackTop.31.ph, i8** %t5315, align 8
  br label %common.ret

L_527:                                            ; preds = %L_526
  %t3301 = add i64 %t3312, %TW64_0.3
  %t3289 = getelementptr inbounds i8, i8* %TP_0.7, i64 8
  %t3290 = bitcast i8* %t3289 to i8**
  %t3294 = bitcast i8* %TP_0.7 to i8**
  br label %L_523

L_526:                                            ; preds = %L_530, %L_529, %L_524
  %cond18 = icmp eq i8* %TP_0.7, inttoptr (i64 1 to i8*)
  br i1 %cond18, label %L_528, label %L_527

L_524:                                            ; preds = %L_523
  tail call void @GC_sequenceCopy(i8* %gcState, i8* %TP_2.0.ph, i64 %TW64_0.3, i8* nonnull %TP_1.1, i64 0, i64 %t3312)
  br label %L_526

L_523:                                            ; preds = %L_523.preheader, %L_527
  %TP_0.7.in = phi i8** [ %t3290, %L_527 ], [ %TP_0.7.in.ph, %L_523.preheader ]
  %TP_1.1.in = phi i8** [ %t3294, %L_527 ], [ %TP_1.1.in.ph, %L_523.preheader ]
  %TW64_0.3 = phi i64 [ %t3301, %L_527 ], [ 0, %L_523.preheader ]
  %TP_1.1 = load i8*, i8** %TP_1.1.in, align 8
  %TP_0.7 = load i8*, i8** %TP_0.7.in, align 8
  %t3310 = getelementptr inbounds i8, i8* %TP_1.1, i64 -16
  %t3311 = bitcast i8* %t3310 to i64*
  %t3312 = load i64, i64* %t3311, align 4
  %t3314 = icmp sgt i64 %t3312, 4
  br i1 %t3314, label %L_524, label %L_529

L_nonZeroLen_5:                                   ; preds = %L_521
  %t3331 = getelementptr inbounds i8, i8* %stackTop.32, i64 24
  %t3332 = bitcast i8* %t3331 to i64*
  store i64 148, i64* %t3332, align 4
  %t3334 = getelementptr inbounds i8, i8* %stackTop.32, i64 32
  store i8* %frontier.32, i8** %t5312, align 8
  store i8* %t3334, i8** %t5315, align 8
  %t3341 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t3375901, i64 21)
  %t3344 = load i8*, i8** %t5312, align 8
  %t3347 = load i8*, i8** %t5315, align 8
  %t3318 = getelementptr inbounds i8, i8* %t3347, i64 -32
  %t3321 = getelementptr inbounds i8, i8* %t3347, i64 -16
  %t3322 = bitcast i8* %t3321 to i8**
  %t3326 = getelementptr inbounds i8, i8* %t3347, i64 -24
  %t3327 = bitcast i8* %t3326 to i8**
  br label %L_523.preheader

L_521:                                            ; preds = %L_520
  %t3349.not = icmp eq i64 %t3375901, 0
  br i1 %t3349.not, label %L_523.preheader, label %L_nonZeroLen_5

L_523.preheader:                                  ; preds = %L_nonZeroLen_5, %L_521
  %TP_0.7.in.ph = phi i8** [ %t3392, %L_521 ], [ %t3327, %L_nonZeroLen_5 ]
  %TP_1.1.in.ph = phi i8** [ %t3402, %L_521 ], [ %t3322, %L_nonZeroLen_5 ]
  %TP_2.0.ph = phi i8* [ getelementptr (i8, i8* @staticHeapM, i64 64), %L_521 ], [ %t3341, %L_nonZeroLen_5 ]
  %stackTop.31.ph = phi i8* [ %stackTop.32, %L_521 ], [ %t3318, %L_nonZeroLen_5 ]
  %frontier.31.ph = phi i8* [ %frontier.32, %L_521 ], [ %t3344, %L_nonZeroLen_5 ]
  br label %L_523

L_520:                                            ; preds = %L_518
  %t3353 = icmp ult i64 %t3375901, 2147483648
  br i1 %t3353, label %L_521, label %L_531

L_519:                                            ; preds = %L_518
  %t3357 = getelementptr inbounds i8, i8* %TP_0.8900, i64 8
  %t3358 = bitcast i8* %t3357 to i8**
  %t3359 = load i8*, i8** %t3358, align 8
  %t3362 = bitcast i8* %TP_0.8900 to i8**
  %t3363 = load i8*, i8** %t3362, align 8
  %t3370 = getelementptr inbounds i8, i8* %t3363, i64 -16
  %t3371 = bitcast i8* %t3370 to i64*
  %t3372 = load i64, i64* %t3371, align 4
  %t3375 = add i64 %t3372, %t3375901
  %t3378 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %t3372, i64 %t3375901)
  %t3379 = extractvalue { i64, i1 } %t3378, 1
  br i1 %t3379, label %L_531, label %L_518

L_518:                                            ; preds = %L_518.preheader, %L_519
  %t3375901 = phi i64 [ %t3375, %L_519 ], [ %t3372896, %L_518.preheader ]
  %TP_0.8900 = phi i8* [ %t3359, %L_519 ], [ %t3399, %L_518.preheader ]
  %cond17 = icmp eq i8* %TP_0.8900, inttoptr (i64 1 to i8*)
  br i1 %cond17, label %L_520, label %L_519

L_515:                                            ; preds = %L_514
  %34 = bitcast i8* %t3418 to i8**
  %t3391 = getelementptr inbounds i8, i8* %stackTop.32, i64 8
  %t3392 = bitcast i8* %t3391 to i8**
  %t3397 = getelementptr inbounds i8, i8* %t3418, i64 8
  %t3398 = bitcast i8* %t3397 to i8**
  %t3399 = load i8*, i8** %t3398, align 8
  store i8* %t3399, i8** %t3392, align 8
  %t3401 = getelementptr inbounds i8, i8* %stackTop.32, i64 16
  %t3402 = bitcast i8* %t3401 to i8**
  %t3409 = load i8*, i8** %34, align 8
  store i8* %t3409, i8** %t3402, align 8
  %cond16 = icmp eq i8* %t3399, inttoptr (i64 1 to i8*)
  br i1 %cond16, label %L_532, label %L_518.preheader

L_518.preheader:                                  ; preds = %L_515
  %t3370894 = getelementptr inbounds i8, i8* %t3409, i64 -16
  %t3371895 = bitcast i8* %t3370894 to i64*
  %t3372896 = load i64, i64* %t3371895, align 4
  br label %L_518

L_514:                                            ; preds = %L_511, %L_512
  %stackTop.32 = phi i8* [ %t3143, %L_512 ], [ %stackTop.33, %L_511 ]
  %frontier.32 = phi i8* [ %t3158, %L_512 ], [ %frontier.33, %L_511 ]
  %t3417 = bitcast i8* %stackTop.32 to i8**
  %t3418 = load i8*, i8** %t3417, align 8
  %cond15 = icmp eq i8* %t3418, inttoptr (i64 1 to i8*)
  br i1 %cond15, label %L_533, label %L_515

L_511.sink.split:                                 ; preds = %L_985, %L_560, %L_570, %L_585, %L_589, %L_400
  %t2822.sink = phi i8* [ %t2822, %L_400 ], [ %t3611, %L_589 ], [ %t3686, %L_585 ], [ %t3874, %L_570 ], [ %t4043, %L_560 ], [ %t5363, %L_985 ]
  %.sink1274 = phi i64 [ 24, %L_400 ], [ 27, %L_589 ], [ 28, %L_585 ], [ 29, %L_570 ], [ 30, %L_560 ], [ 37, %L_985 ]
  %stackTop.33.ph = phi i8* [ %t2801, %L_400 ], [ %t3607, %L_589 ], [ %t3682, %L_585 ], [ %t3870, %L_570 ], [ %t4083, %L_560 ], [ %t5393, %L_985 ]
  %frontier.33.ph = phi i8* [ %t2776, %L_400 ], [ %t3593, %L_589 ], [ %t3657, %L_585 ], [ %t3845, %L_570 ], [ %t4058, %L_560 ], [ %t5379, %L_985 ]
  %t2806 = bitcast i8* %t2822.sink to i64*
  store i64 %.sink1274, i64* %t2806, align 4
  br label %L_511

L_511:                                            ; preds = %L_511.sink.split, %doSwitchNextBlock
  %stackTop.33 = phi i8* [ %stackTop.0, %doSwitchNextBlock ], [ %stackTop.33.ph, %L_511.sink.split ]
  %frontier.33 = phi i8* [ %frontier.0, %doSwitchNextBlock ], [ %frontier.33.ph, %L_511.sink.split ]
  %t3422 = load i8*, i8** %t4312, align 8
  %t3424.not = icmp ult i8* %t3422, %stackTop.33
  br i1 %t3424.not, label %L_512, label %L_514

exnMessage_0:                                     ; preds = %doSwitchNextBlock, %L_556
  %stackTop.34 = phi i8* [ %t4138, %L_556 ], [ %stackTop.0, %doSwitchNextBlock ]
  %frontier.34 = phi i8* [ %frontier.41, %L_556 ], [ %frontier.0, %doSwitchNextBlock ]
  %t4306 = getelementptr inbounds i8, i8* %stackTop.34, i64 16
  %t4307 = bitcast i8* %t4306 to i64*
  %t4310 = load i64, i64* %t4081, align 4
  store i64 %t4310, i64* %t4307, align 4
  %t4313 = load i8*, i8** %t4312, align 8
  %t4315.not = icmp ult i8* %t4313, %stackTop.34
  br i1 %t4315.not, label %L_535, label %L_537

L_535:                                            ; preds = %exnMessage_0
  %t3430 = getelementptr inbounds i8, i8* %stackTop.34, i64 40
  %t3431 = bitcast i8* %t3430 to i64*
  store i64 156, i64* %t3431, align 4
  %t3433 = getelementptr inbounds i8, i8* %stackTop.34, i64 48
  store i8* %frontier.34, i8** %t5312, align 8
  store i8* %t3433, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t3443 = load i8*, i8** %t5312, align 8
  %t3446 = load i8*, i8** %t5315, align 8
  %t3428 = getelementptr inbounds i8, i8* %t3446, i64 -48
  br label %L_537

L_594:                                            ; preds = %L_1820
  %t3450 = getelementptr inbounds i8, i8* %stackTop.41, i64 40
  %t3451 = bitcast i8* %t3450 to i64*
  store i64 156, i64* %t3451, align 4
  %t3453 = getelementptr inbounds i8, i8* %stackTop.41, i64 48
  store i8* %frontier.41, i8** %t5312, align 8
  store i8* %t3453, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t3463 = load i8*, i8** %t5312, align 8
  %t3466 = load i8*, i8** %t5315, align 8
  %t3448 = getelementptr inbounds i8, i8* %t3466, i64 -48
  %t3469.phi.trans.insert = bitcast i8* %t3448 to i8**
  %t3470.pre = load i8*, i8** %t3469.phi.trans.insert, align 8
  br label %L_596

L_596:                                            ; preds = %L_1820, %L_594
  %t3470 = phi i8* [ %t3470.pre, %L_594 ], [ %36, %L_1820 ]
  %stackTop.35 = phi i8* [ %t3448, %L_594 ], [ %stackTop.41, %L_1820 ]
  %frontier.35 = phi i8* [ %t3463, %L_594 ], [ %frontier.41, %L_1820 ]
  %t3471 = getelementptr inbounds i8, i8* %t3470, i64 8
  %t3472 = bitcast i8* %t3471 to i8**
  %t3473 = load i8*, i8** %t3472, align 8
  br label %L_585

L_1820:                                           ; preds = %L_580, %L_577
  %.lcssa = phi i32* [ bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 4832) to i32*), %L_577 ], [ %t3706563, %L_580 ]
  %35 = bitcast i8* %stackTop.41 to i32**
  store i32* %.lcssa, i32** %35, align 8
  %t3480 = load i8*, i8** %t5411, align 8
  %t3482.not = icmp ult i8* %t3480, %frontier.41
  %36 = bitcast i32* %.lcssa to i8*
  br i1 %t3482.not, label %L_594, label %L_596

L_582:                                            ; preds = %L_581
  %t3488 = getelementptr inbounds i8, i8* %stackTop.41, i64 40
  %t3489 = bitcast i8* %t3488 to i64*
  store i64 157, i64* %t3489, align 4
  %t3491 = getelementptr inbounds i8, i8* %stackTop.41, i64 48
  store i8* %frontier.41, i8** %t5312, align 8
  store i8* %t3491, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t3501 = load i8*, i8** %t5312, align 8
  %t3504 = load i8*, i8** %t5315, align 8
  %t3486 = getelementptr inbounds i8, i8* %t3504, i64 -48
  br label %L_585

L_587:                                            ; preds = %L_1818
  %t3508 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t3509 = bitcast i8* %t3508 to i64*
  store i64 156, i64* %t3509, align 4
  store i8* %frontier.0, i8** %t5312, align 8
  store i8* %stackTop.0, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t3521 = load i8*, i8** %t5312, align 8
  %t3524 = load i8*, i8** %t5315, align 8
  %t3506 = getelementptr inbounds i8, i8* %t3524, i64 -48
  %.pre1094 = bitcast i8* %t3506 to i8**
  br label %L_589

L_1819:                                           ; preds = %doSwitchNextBlock
  %t3526 = getelementptr inbounds i8, i8* %stackTop.0, i64 -48
  %t3529 = bitcast i8* %stackTop.0 to i8**
  %t3530 = load i8*, i8** %t3529, align 8
  %t3534 = getelementptr inbounds i8, i8* %stackTop.0, i64 -32
  %t3535 = bitcast i8* %t3534 to i64*
  %t3536 = load i64, i64* %t3535, align 4
  store i64 %t3536, i64* %t4081, align 4
  %t3539 = bitcast i8* %t3526 to i8**
  store i8* %t3530, i8** %t3539, align 8
  %t3542 = getelementptr inbounds i8, i8* %stackTop.0, i64 -56
  br label %doSwitchNextBlock.backedge.sink.split

L_589:                                            ; preds = %L_1818, %L_587
  %t3559.pre-phi = phi i8** [ %t3619, %L_1818 ], [ %.pre1094, %L_587 ]
  %stackTop.36 = phi i8* [ %t3616, %L_1818 ], [ %t3506, %L_587 ]
  %frontier.36 = phi i8* [ %frontier.0, %L_1818 ], [ %t3521, %L_587 ]
  %t3546 = getelementptr inbounds i8, i8* %frontier.36, i64 8
  %t3551 = bitcast i8* %frontier.36 to i64*
  store i64 123, i64* %t3551, align 4
  %t3556 = bitcast i8* %t3546 to i8**
  %t3560 = load i8*, i8** %t3559.pre-phi, align 8
  store i8* %t3560, i8** %t3556, align 8
  %t3562 = getelementptr inbounds i8, i8* %frontier.36, i64 16
  %t3563 = bitcast i8* %t3562 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t3563, align 8
  %t3566 = getelementptr inbounds i8, i8* %frontier.36, i64 32
  %t3570 = getelementptr inbounds i8, i8* %frontier.36, i64 24
  %t3571 = bitcast i8* %t3570 to i64*
  store i64 123, i64* %t3571, align 4
  %t3576 = bitcast i8* %t3566 to i8**
  %t3578 = getelementptr inbounds i8, i8* %stackTop.36, i64 8
  %t3579 = bitcast i8* %t3578 to i8**
  %t3580 = load i8*, i8** %t3579, align 8
  store i8* %t3580, i8** %t3576, align 8
  %t3582 = getelementptr inbounds i8, i8* %frontier.36, i64 40
  %t3583 = bitcast i8* %t3582 to i8**
  store i8* %t3546, i8** %t3583, align 8
  %t3586 = getelementptr inbounds i8, i8* %frontier.36, i64 56
  %t3590 = getelementptr inbounds i8, i8* %frontier.36, i64 48
  %t3591 = bitcast i8* %t3590 to i64*
  store i64 123, i64* %t3591, align 4
  %t3593 = getelementptr inbounds i8, i8* %frontier.36, i64 72
  %t3596 = bitcast i8* %t3586 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4384), i8** %t3596, align 8
  %t3600 = getelementptr inbounds i8, i8* %frontier.36, i64 64
  %t3601 = bitcast i8* %t3600 to i8**
  store i8* %t3566, i8** %t3601, align 8
  %t3604 = getelementptr inbounds i8, i8* %stackTop.36, i64 24
  %t3605 = bitcast i8* %t3604 to i64*
  store i64 3, i64* %t3605, align 4
  %t3607 = getelementptr inbounds i8, i8* %stackTop.36, i64 48
  %t3608 = bitcast i8* %t3607 to i8**
  store i8* %t3586, i8** %t3608, align 8
  %t3611 = getelementptr inbounds i8, i8* %stackTop.36, i64 40
  br label %L_511.sink.split

L_1818:                                           ; preds = %doSwitchNextBlock
  %t3616 = getelementptr inbounds i8, i8* %stackTop.0, i64 -48
  %t3619 = bitcast i8* %t3616 to i8**
  %t3622 = bitcast i8* %stackTop.0 to i8**
  %t3623 = load i8*, i8** %t3622, align 8
  store i8* %t3623, i8** %t3619, align 8
  %t3626 = load i8*, i8** %t5411, align 8
  %t3628.not = icmp ult i8* %t3626, %frontier.0
  br i1 %t3628.not, label %L_587, label %L_589

L_585:                                            ; preds = %L_581, %L_582, %L_596
  %TP_0.9 = phi i8* [ %t3473, %L_596 ], [ getelementptr (i8, i8* @staticHeapI, i64 4448), %L_581 ], [ getelementptr (i8, i8* @staticHeapI, i64 4448), %L_582 ]
  %stackTop.37 = phi i8* [ %stackTop.35, %L_596 ], [ %stackTop.41, %L_581 ], [ %t3486, %L_582 ]
  %frontier.37 = phi i8* [ %frontier.35, %L_596 ], [ %frontier.41, %L_581 ], [ %t3501, %L_582 ]
  %t3632 = getelementptr inbounds i8, i8* %frontier.37, i64 8
  %t3637 = bitcast i8* %frontier.37 to i64*
  store i64 123, i64* %t3637, align 4
  %t3642 = bitcast i8* %t3632 to i8**
  store i8* %TP_0.9, i8** %t3642, align 8
  %t3645 = getelementptr inbounds i8, i8* %frontier.37, i64 16
  %t3646 = bitcast i8* %t3645 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 8984), i8** %t3646, align 8
  %t3650 = getelementptr inbounds i8, i8* %frontier.37, i64 32
  %t3654 = getelementptr inbounds i8, i8* %frontier.37, i64 24
  %t3655 = bitcast i8* %t3654 to i64*
  store i64 123, i64* %t3655, align 4
  %t3657 = getelementptr inbounds i8, i8* %frontier.37, i64 48
  %t3660 = bitcast i8* %t3650 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4416), i8** %t3660, align 8
  %t3664 = getelementptr inbounds i8, i8* %frontier.37, i64 40
  %t3665 = bitcast i8* %t3664 to i8**
  store i8* %t3632, i8** %t3665, align 8
  %t3668 = getelementptr inbounds i8, i8* %stackTop.37, i64 24
  %t3669 = bitcast i8* %t3668 to i64*
  store i64 2, i64* %t3669, align 4
  %t3671 = getelementptr inbounds i8, i8* %stackTop.37, i64 32
  %t3675 = load i8*, i8** %t4075, align 8
  %t3676 = ptrtoint i8* %t3671 to i64
  %t3677 = ptrtoint i8* %t3675 to i64
  %t3678 = sub i64 %t3676, %t3677
  store i64 %t3678, i64* %t4081, align 4
  %t3682 = getelementptr inbounds i8, i8* %stackTop.37, i64 48
  %t3683 = bitcast i8* %t3682 to i8**
  store i8* %t3650, i8** %t3683, align 8
  %t3686 = getelementptr inbounds i8, i8* %stackTop.37, i64 40
  br label %L_511.sink.split

L_581:                                            ; preds = %L_579
  %t3694 = load i8*, i8** %t5411, align 8
  %t3696.not = icmp ult i8* %t3694, %frontier.41
  br i1 %t3696.not, label %L_582, label %L_585

L_580:                                            ; preds = %L_579
  %t3700 = getelementptr inbounds i8, i8* %TP_0.10870, i64 8
  %t3701 = bitcast i8* %t3700 to i8**
  %t3702 = load i8*, i8** %t3701, align 8
  %37 = bitcast i8* %TP_0.10870 to i32**
  %t3706563 = load i32*, i32** %37, align 8
  %t3714 = load i32, i32* %t3706563, align 4
  %t3717.not = icmp eq i32 %t3714, %t3727
  br i1 %t3717.not, label %L_1820, label %L_579

L_579:                                            ; preds = %L_577, %L_580
  %TP_0.10870 = phi i8* [ %t3702, %L_580 ], [ getelementptr (i8, i8* @staticHeapI, i64 8576), %L_577 ]
  %cond8 = icmp eq i8* %TP_0.10870, inttoptr (i64 1 to i8*)
  br i1 %cond8, label %L_581, label %L_580

L_577:                                            ; preds = %L_576
  %t4298.le = bitcast i8* %t4297 to i8**
  %t3726 = bitcast i8* %t3768 to i32*
  %t3727 = load i32, i32* %t3726, align 4
  %t3736 = getelementptr inbounds i8, i8* %t3768, i64 8
  %t3737 = bitcast i8* %t3736 to i8**
  %t3738 = load i8*, i8** %t3737, align 8
  store i8* %t3738, i8** %t4298.le, align 8
  %t3714868 = load i32, i32* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 4832) to i32*), align 4
  %t3717.not869 = icmp eq i32 %t3714868, %t3727
  br i1 %t3717.not869, label %L_1820, label %L_579

L_576:                                            ; preds = %L_573
  %t3747 = getelementptr inbounds i8, i8* %t3768, i64 -8
  %t3748 = bitcast i8* %t3747 to i64*
  %t3749 = load i64, i64* %t3748, align 4
  %t3751.mask = and i64 %t3749, -2
  %cond7 = icmp eq i64 %t3751.mask, 124
  br i1 %cond7, label %L_577, label %L_574

L_574:                                            ; preds = %L_576, %L_573
  %cond6 = icmp eq i8* %TP_0.11, inttoptr (i64 1 to i8*)
  br i1 %cond6, label %L_543, label %L_539.backedge

L_573:                                            ; preds = %L_539
  %t3768 = load i8*, i8** %t4262.phi.trans.insert, align 8
  %t3769 = ptrtoint i8* %t3768 to i64
  %t3770 = and i64 %t3769, 3
  %cond5 = icmp eq i64 %t3770, 0
  br i1 %cond5, label %L_576, label %L_574

L_568:                                            ; preds = %L_567
  %t3775 = getelementptr inbounds i8, i8* %stackTop.41, i64 40
  %t3776 = bitcast i8* %t3775 to i64*
  store i64 155, i64* %t3776, align 4
  %t3778 = getelementptr inbounds i8, i8* %stackTop.41, i64 48
  store i8* %frontier.41, i8** %t5312, align 8
  store i8* %t3778, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t3788 = load i8*, i8** %t5312, align 8
  %t3791 = load i8*, i8** %t5315, align 8
  %t3773 = getelementptr inbounds i8, i8* %t3791, i64 -48
  br label %L_570

L_1817:                                           ; preds = %doSwitchNextBlock
  %t3793 = getelementptr inbounds i8, i8* %stackTop.0, i64 -48
  %t3796 = bitcast i8* %stackTop.0 to i8**
  %t3797 = load i8*, i8** %t3796, align 8
  %t3801 = getelementptr inbounds i8, i8* %stackTop.0, i64 -32
  %t3802 = bitcast i8* %t3801 to i64*
  %t3803 = load i64, i64* %t3802, align 4
  store i64 %t3803, i64* %t4081, align 4
  %t3806 = bitcast i8* %t3793 to i8**
  store i8* %t3797, i8** %t3806, align 8
  %t3809 = getelementptr inbounds i8, i8* %stackTop.0, i64 -56
  br label %doSwitchNextBlock.backedge.sink.split

L_570:                                            ; preds = %L_567, %L_568
  %stackTop.39 = phi i8* [ %t3773, %L_568 ], [ %stackTop.41, %L_567 ]
  %frontier.39 = phi i8* [ %t3788, %L_568 ], [ %frontier.41, %L_567 ]
  %38 = bitcast i8* %stackTop.39 to i8***
  %t3815568 = load i8**, i8*** %38, align 8
  %t3819 = load i8*, i8** %t3815568, align 8
  %t3821 = getelementptr inbounds i8, i8* %frontier.39, i64 8
  %t3826 = bitcast i8* %frontier.39 to i64*
  store i64 123, i64* %t3826, align 4
  %t3831 = bitcast i8* %t3821 to i8**
  store i8* %t3819, i8** %t3831, align 8
  %t3834 = getelementptr inbounds i8, i8* %frontier.39, i64 16
  %t3835 = bitcast i8* %t3834 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t3835, align 8
  %t3838 = getelementptr inbounds i8, i8* %frontier.39, i64 32
  %t3842 = getelementptr inbounds i8, i8* %frontier.39, i64 24
  %t3843 = bitcast i8* %t3842 to i64*
  store i64 123, i64* %t3843, align 4
  %t3845 = getelementptr inbounds i8, i8* %frontier.39, i64 48
  %t3848 = bitcast i8* %t3838 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4488), i8** %t3848, align 8
  %t3852 = getelementptr inbounds i8, i8* %frontier.39, i64 40
  %t3853 = bitcast i8* %t3852 to i8**
  store i8* %t3821, i8** %t3853, align 8
  %t3856 = getelementptr inbounds i8, i8* %stackTop.39, i64 24
  %t3857 = bitcast i8* %t3856 to i64*
  store i64 4, i64* %t3857, align 4
  %t3859 = getelementptr inbounds i8, i8* %stackTop.39, i64 32
  %t3863 = load i8*, i8** %t4075, align 8
  %t3864 = ptrtoint i8* %t3859 to i64
  %t3865 = ptrtoint i8* %t3863 to i64
  %t3866 = sub i64 %t3864, %t3865
  store i64 %t3866, i64* %t4081, align 4
  %t3870 = getelementptr inbounds i8, i8* %stackTop.39, i64 48
  %t3871 = bitcast i8* %t3870 to i8**
  store i8* %t3838, i8** %t3871, align 8
  %t3874 = getelementptr inbounds i8, i8* %stackTop.39, i64 40
  br label %L_511.sink.split

L_567:                                            ; preds = %L_566
  %t3880 = load i8*, i8** %t5411, align 8
  %t3882.not = icmp ult i8* %t3880, %frontier.41
  br i1 %t3882.not, label %L_568, label %L_570

L_566:                                            ; preds = %L_563
  %t3889 = getelementptr inbounds i8, i8* %t3910, i64 -8
  %t3890 = bitcast i8* %t3889 to i64*
  %t3891 = load i64, i64* %t3890, align 4
  %t3893.mask = and i64 %t3891, -2
  %cond11 = icmp eq i64 %t3893.mask, 128
  br i1 %cond11, label %L_567, label %L_564

L_564:                                            ; preds = %L_566, %L_563
  %cond10 = icmp eq i8* %TP_0.11, inttoptr (i64 1 to i8*)
  br i1 %cond10, label %L_543, label %L_539.backedge

L_563:                                            ; preds = %L_539
  %t3910 = load i8*, i8** %t4262.phi.trans.insert, align 8
  %t3911 = ptrtoint i8* %t3910 to i64
  %t3912 = and i64 %t3911, 3
  %cond9 = icmp eq i64 %t3912, 0
  br i1 %cond9, label %L_566, label %L_564

L_558:                                            ; preds = %L_557
  %t3917 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t3918 = bitcast i8* %t3917 to i64*
  store i64 154, i64* %t3918, align 4
  store i8* %frontier.0, i8** %t5312, align 8
  store i8* %stackTop.0, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t3930 = load i8*, i8** %t5312, align 8
  %t3933 = load i8*, i8** %t5315, align 8
  %t3915 = getelementptr inbounds i8, i8* %t3933, i64 -56
  %.pre1095 = bitcast i8* %t3915 to i8**
  br label %L_560

L_1816:                                           ; preds = %doSwitchNextBlock
  %t3935 = getelementptr inbounds i8, i8* %stackTop.0, i64 -48
  %t3938 = bitcast i8* %stackTop.0 to i8**
  %t3939 = load i8*, i8** %t3938, align 8
  %t3943 = getelementptr inbounds i8, i8* %stackTop.0, i64 -32
  %t3944 = bitcast i8* %t3943 to i64*
  %t3945 = load i64, i64* %t3944, align 4
  store i64 %t3945, i64* %t4081, align 4
  %t3948 = bitcast i8* %t3935 to i8**
  store i8* %t3939, i8** %t3948, align 8
  %t3951 = getelementptr inbounds i8, i8* %stackTop.0, i64 -56
  br label %doSwitchNextBlock.backedge.sink.split

L_560:                                            ; preds = %L_557, %L_558
  %t3968.pre-phi = phi i8** [ %t4095, %L_557 ], [ %.pre1095, %L_558 ]
  %stackTop.40 = phi i8* [ %t4092, %L_557 ], [ %t3915, %L_558 ]
  %frontier.40 = phi i8* [ %frontier.0, %L_557 ], [ %t3930, %L_558 ]
  %t3955 = getelementptr inbounds i8, i8* %frontier.40, i64 8
  %t3960 = bitcast i8* %frontier.40 to i64*
  store i64 123, i64* %t3960, align 4
  %t3965 = bitcast i8* %t3955 to i8**
  %t3969 = load i8*, i8** %t3968.pre-phi, align 8
  store i8* %t3969, i8** %t3965, align 8
  %t3971 = getelementptr inbounds i8, i8* %frontier.40, i64 16
  %t3972 = bitcast i8* %t3971 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t3972, align 8
  %t3975 = getelementptr inbounds i8, i8* %frontier.40, i64 32
  %t3979 = getelementptr inbounds i8, i8* %frontier.40, i64 24
  %t3980 = bitcast i8* %t3979 to i64*
  store i64 123, i64* %t3980, align 4
  %t3985 = bitcast i8* %t3975 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4520), i8** %t3985, align 8
  %t3989 = getelementptr inbounds i8, i8* %frontier.40, i64 40
  %t3990 = bitcast i8* %t3989 to i8**
  store i8* %t3955, i8** %t3990, align 8
  %t3993 = getelementptr inbounds i8, i8* %frontier.40, i64 56
  %t3997 = getelementptr inbounds i8, i8* %frontier.40, i64 48
  %t3998 = bitcast i8* %t3997 to i64*
  store i64 123, i64* %t3998, align 4
  %t4003 = bitcast i8* %t3993 to i8**
  %t4005 = getelementptr inbounds i8, i8* %stackTop.40, i64 8
  %t4006 = bitcast i8* %t4005 to i8**
  %t4007 = load i8*, i8** %t4006, align 8
  store i8* %t4007, i8** %t4003, align 8
  %t4009 = getelementptr inbounds i8, i8* %frontier.40, i64 64
  %t4010 = bitcast i8* %t4009 to i8**
  store i8* %t3975, i8** %t4010, align 8
  %t4013 = getelementptr inbounds i8, i8* %frontier.40, i64 80
  %t4017 = getelementptr inbounds i8, i8* %frontier.40, i64 72
  %t4018 = bitcast i8* %t4017 to i64*
  store i64 123, i64* %t4018, align 4
  %t4023 = bitcast i8* %t4013 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4560), i8** %t4023, align 8
  %t4027 = getelementptr inbounds i8, i8* %frontier.40, i64 88
  %t4028 = bitcast i8* %t4027 to i8**
  store i8* %t3993, i8** %t4028, align 8
  %t4031 = getelementptr inbounds i8, i8* %frontier.40, i64 104
  %t4035 = getelementptr inbounds i8, i8* %frontier.40, i64 96
  %t4036 = bitcast i8* %t4035 to i64*
  store i64 123, i64* %t4036, align 4
  %t4041 = bitcast i8* %t4031 to i8**
  %t4043 = getelementptr inbounds i8, i8* %stackTop.40, i64 40
  %t4044 = bitcast i8* %t4043 to i8**
  %t4045 = load i8*, i8** %t4044, align 8
  store i8* %t4045, i8** %t4041, align 8
  %t4047 = getelementptr inbounds i8, i8* %frontier.40, i64 112
  %t4048 = bitcast i8* %t4047 to i8**
  store i8* %t4013, i8** %t4048, align 8
  %t4051 = getelementptr inbounds i8, i8* %frontier.40, i64 128
  %t4055 = getelementptr inbounds i8, i8* %frontier.40, i64 120
  %t4056 = bitcast i8* %t4055 to i64*
  store i64 123, i64* %t4056, align 4
  %t4058 = getelementptr inbounds i8, i8* %frontier.40, i64 144
  %t4061 = bitcast i8* %t4051 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4592), i8** %t4061, align 8
  %t4065 = getelementptr inbounds i8, i8* %frontier.40, i64 136
  %t4066 = bitcast i8* %t4065 to i8**
  store i8* %t4031, i8** %t4066, align 8
  %t4069 = getelementptr inbounds i8, i8* %stackTop.40, i64 24
  %t4070 = bitcast i8* %t4069 to i64*
  store i64 5, i64* %t4070, align 4
  %t4072 = getelementptr inbounds i8, i8* %stackTop.40, i64 32
  %t4076 = load i8*, i8** %t4075, align 8
  %t4077 = ptrtoint i8* %t4072 to i64
  %t4078 = ptrtoint i8* %t4076 to i64
  %t4079 = sub i64 %t4077, %t4078
  store i64 %t4079, i64* %t4081, align 4
  %t4083 = getelementptr inbounds i8, i8* %stackTop.40, i64 48
  %t4084 = bitcast i8* %t4083 to i8**
  store i8* %t4051, i8** %t4084, align 8
  br label %L_511.sink.split

L_557:                                            ; preds = %doSwitchNextBlock
  %t4092 = getelementptr inbounds i8, i8* %stackTop.0, i64 -56
  %t4095 = bitcast i8* %t4092 to i8**
  %t4098 = bitcast i8* %stackTop.0 to i8**
  %t4099 = load i8*, i8** %t4098, align 8
  store i8* %t4099, i8** %t4095, align 8
  %t4102 = load i8*, i8** %t5411, align 8
  %t4104.not = icmp ult i8* %t4102, %frontier.0
  br i1 %t4104.not, label %L_558, label %L_560

L_556:                                            ; preds = %L_555
  %t4115 = getelementptr inbounds i8, i8* %t4280, i64 16
  %t4116 = bitcast i8* %t4115 to i8**
  %t4117 = load i8*, i8** %t4116, align 8
  store i8* %t4117, i8** %t4298, align 8
  %t4119 = getelementptr inbounds i8, i8* %stackTop.41, i64 40
  %t4120 = bitcast i8* %t4119 to i8**
  %t4126 = getelementptr inbounds i8, i8* %t4280, i64 8
  %t4127 = bitcast i8* %t4126 to i8**
  %t4128 = load i8*, i8** %t4127, align 8
  store i8* %t4128, i8** %t4120, align 8
  %t4132569.cast = bitcast i8* %t4280 to i8**
  %t4136 = load i8*, i8** %t4132569.cast, align 8
  %t4138 = getelementptr inbounds i8, i8* %stackTop.41, i64 56
  %t4139 = bitcast i8* %t4138 to i8**
  store i8* %t4136, i8** %t4139, align 8
  %t4142 = getelementptr inbounds i8, i8* %stackTop.41, i64 64
  %t4143 = bitcast i8* %t4142 to i8**
  store i8* %TP_1.4, i8** %t4143, align 8
  %t4146 = getelementptr inbounds i8, i8* %stackTop.41, i64 48
  %t4147 = bitcast i8* %t4146 to i64*
  store i64 31, i64* %t4147, align 4
  br label %exnMessage_0

L_555:                                            ; preds = %L_540
  %t4154 = getelementptr inbounds i8, i8* %t4280, i64 -8
  %t4155 = bitcast i8* %t4154 to i64*
  %t4156 = load i64, i64* %t4155, align 4
  %t4158.mask = and i64 %t4156, -2
  %cond14 = icmp eq i64 %t4158.mask, 126
  br i1 %cond14, label %L_556, label %L_541

L_554:                                            ; preds = %L_543
  store i8* getelementptr (i8, i8* @staticHeapI, i64 3984), i8** %t4262.pre-phi, align 8
  %t4166 = getelementptr inbounds i8, i8* %stackTop.41, i64 -8
  br label %doSwitchNextBlock.backedge.sink.split

L_553:                                            ; preds = %L_543
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4016), i8** %t4262.pre-phi, align 8
  %t4175 = getelementptr inbounds i8, i8* %stackTop.41, i64 -8
  br label %doSwitchNextBlock.backedge.sink.split

L_552:                                            ; preds = %L_543
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4056), i8** %t4262.pre-phi, align 8
  %t4184 = getelementptr inbounds i8, i8* %stackTop.41, i64 -8
  br label %doSwitchNextBlock.backedge.sink.split

L_551:                                            ; preds = %L_543
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4096), i8** %t4262.pre-phi, align 8
  %t4193 = getelementptr inbounds i8, i8* %stackTop.41, i64 -8
  br label %doSwitchNextBlock.backedge.sink.split

L_550:                                            ; preds = %L_543
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4128), i8** %t4262.pre-phi, align 8
  %t4202 = getelementptr inbounds i8, i8* %stackTop.41, i64 -8
  br label %doSwitchNextBlock.backedge.sink.split

L_549:                                            ; preds = %L_543
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4160), i8** %t4262.pre-phi, align 8
  %t4211 = getelementptr inbounds i8, i8* %stackTop.41, i64 -8
  br label %doSwitchNextBlock.backedge.sink.split

L_548:                                            ; preds = %L_544
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4624), i8** %t4262.pre-phi, align 8
  %t4220 = getelementptr inbounds i8, i8* %stackTop.41, i64 -8
  br label %doSwitchNextBlock.backedge.sink.split

L_547:                                            ; preds = %L_544
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4224), i8** %t4262.pre-phi, align 8
  %t4229 = getelementptr inbounds i8, i8* %stackTop.41, i64 -8
  br label %doSwitchNextBlock.backedge.sink.split

L_546:                                            ; preds = %L_544
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4256), i8** %t4262.pre-phi, align 8
  %t4238 = getelementptr inbounds i8, i8* %stackTop.41, i64 -8
  br label %doSwitchNextBlock.backedge.sink.split

L_545:                                            ; preds = %L_544
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4352), i8** %t4262.pre-phi, align 8
  %t4247 = getelementptr inbounds i8, i8* %stackTop.41, i64 -8
  br label %doSwitchNextBlock.backedge.sink.split

L_544:                                            ; preds = %L_543
  %t4254 = getelementptr inbounds i8, i8* %t4263, i64 -8
  %t4255 = bitcast i8* %t4254 to i64*
  %t4256 = load i64, i64* %t4255, align 4
  %t4258 = lshr i64 %t4256, 1
  %trunc562 = trunc i64 %t4258 to i63
  switch i63 %trunc562, label %L_1954 [
    i63 62, label %L_545
    i63 63, label %L_546
    i63 64, label %L_547
    i63 65, label %L_548
  ]

L_1954:                                           ; preds = %L_544
  unreachable

L_543.loopexit933:                                ; preds = %L_537
  %t4262.phi.trans.insert.le = bitcast i8* %stackTop.41 to i8**
  %t4263.pre = load i8*, i8** %t4262.phi.trans.insert.le, align 8
  br label %L_543

L_543:                                            ; preds = %L_541, %L_564, %L_574, %L_543.loopexit933
  %t4263 = phi i8* [ %t4263.pre, %L_543.loopexit933 ], [ %t3768, %L_574 ], [ %t3910, %L_564 ], [ %t4280, %L_541 ]
  %t4262.pre-phi = bitcast i8* %stackTop.41 to i8**
  %t4264 = ptrtoint i8* %t4263 to i64
  switch i64 %t4264, label %L_544 [
    i64 1, label %L_549
    i64 2, label %L_550
    i64 3, label %L_551
    i64 5, label %L_552
    i64 6, label %L_553
    i64 7, label %L_554
  ]

L_541:                                            ; preds = %L_555, %L_540
  %cond13 = icmp eq i8* %TP_0.11, inttoptr (i64 1 to i8*)
  br i1 %cond13, label %L_543, label %L_539.backedge

L_539.backedge:                                   ; preds = %L_541, %L_564, %L_574
  br label %L_539

L_540:                                            ; preds = %L_539
  %t4280 = load i8*, i8** %t4262.phi.trans.insert, align 8
  %t4281 = ptrtoint i8* %t4280 to i64
  %t4282 = and i64 %t4281, 3
  %cond12 = icmp eq i64 %t4282, 0
  br i1 %cond12, label %L_555, label %L_541

L_539:                                            ; preds = %L_537, %L_539.backedge
  %TP_1.4.in.in = phi i8* [ %TP_0.11, %L_539.backedge ], [ %t4302, %L_537 ]
  %TP_0.11.in.in = getelementptr inbounds i8, i8* %TP_1.4.in.in, i64 8
  %TP_1.4.in = bitcast i8* %TP_1.4.in.in to i8**
  %TP_1.4 = load i8*, i8** %TP_1.4.in, align 8
  %TP_0.11.in = bitcast i8* %TP_0.11.in.in to i8**
  %TP_0.11 = load i8*, i8** %TP_0.11.in, align 8
  %t4285 = ptrtoint i8* %TP_1.4 to i64
  switch i64 %t4285, label %L_540 [
    i64 1, label %L_563
    i64 2, label %L_573
  ]

L_537:                                            ; preds = %exnMessage_0, %L_535
  %stackTop.41 = phi i8* [ %t3428, %L_535 ], [ %stackTop.34, %exnMessage_0 ]
  %frontier.41 = phi i8* [ %t3443, %L_535 ], [ %frontier.34, %exnMessage_0 ]
  %t4297 = getelementptr inbounds i8, i8* %stackTop.41, i64 8
  %t4298 = bitcast i8* %t4297 to i8**
  %39 = bitcast i8* %t4297 to i8***
  %t4299561 = load i8**, i8*** %39, align 8
  %t4302 = load i8*, i8** %t4299561, align 8
  %cond4 = icmp eq i8* %t4302, inttoptr (i64 1 to i8*)
  %t4262.phi.trans.insert = bitcast i8* %stackTop.41 to i8**
  br i1 %cond4, label %L_543.loopexit933, label %L_539

L_269:                                            ; preds = %L_268
  %t4321 = getelementptr inbounds i8, i8* %stackTop.47, i64 16
  %t4322 = bitcast i8* %t4321 to i64*
  store i64 153, i64* %t4322, align 4
  %t4324 = getelementptr inbounds i8, i8* %stackTop.47, i64 24
  store i8* %frontier.47, i8** %t5312, align 8
  store i8* %t4324, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4334 = load i8*, i8** %t5312, align 8
  %t4337 = load i8*, i8** %t5315, align 8
  %t4319 = getelementptr inbounds i8, i8* %t4337, i64 -24
  br label %L_271

L_343:                                            ; preds = %L_271
  %t4345 = lshr i64 %t5106, 1
  %t4347 = trunc i64 %t4345 to i32
  %t4349 = tail call i64 @Posix_IO_lseek(i32 %t4347, i64 0, i32 1)
  br label %L_276

L_341:                                            ; preds = %L_273
  %t4380 = tail call i64 @Posix_IO_writeChar8Arr(i32 1, i8* %t5078, i32 %t5062, i64 %t5080)
  br label %L_276

L_339:                                            ; preds = %L_324
  %t4388 = lshr i64 %t4527, 1
  %t4390 = trunc i64 %t4388 to i32
  %t4392 = tail call i64 @Posix_IO_lseek(i32 %t4390, i64 0, i32 1)
  br label %L_329

L_337:                                            ; preds = %L_326
  %t4423 = tail call i64 @Posix_IO_writeChar8Arr(i32 1, i8* %t4499, i32 %t4483, i64 %t4501)
  br label %L_329

L_335:                                            ; preds = %L_333
  %t4427 = add i32 %t4434, -1
  store i32 %t4427, i32* %t2079, align 4
  %t4695.not = icmp eq i32 %t4439, 4
  br i1 %t4695.not, label %L_306, label %raiseSys_0

L_333:                                            ; preds = %L_329
  %t4439 = tail call i32 @Posix_Error_getErrno()
  %t4434 = load i32, i32* %t2079, align 4
  %t4436.not = icmp eq i32 %t4434, 0
  br i1 %t4436.not, label %L_280, label %L_335

L_331:                                            ; preds = %L_330
  %t4443 = add i32 %t4466, -1
  store i32 %t4443, i32* %t2079, align 4
  %t4449 = bitcast i8* %stackTop.46 to i64*
  store i64 %TW64_0.5, i64* %t4449, align 4
  %t4452 = getelementptr inbounds i8, i8* %stackTop.46, i64 -8
  %t4453 = bitcast i8* %t4452 to i64*
  %t4454 = load i64, i64* %t4453, align 4
  %t4455 = getelementptr inbounds [84 x i64 (i8*, i8*, i8*, i64)*], [84 x i64 (i8*, i8*, i8*, i64)*]* @nextChunks, i64 0, i64 %t4454
  %t4456 = load i64 (i8*, i8*, i8*, i64)*, i64 (i8*, i8*, i8*, i64)** %t4455, align 8
  %t4457 = icmp eq i64 (i8*, i8*, i8*, i64)* %t4456, @Chunk_11
  br i1 %t4457, label %doSwitchNextBlock.backedge, label %L_1958

L_1958:                                           ; preds = %L_331
  store i8* %frontier.46, i8** %t5312, align 8
  store i8* %stackTop.46, i8** %t5315, align 8
  br label %common.ret

L_330:                                            ; preds = %L_329
  %t4466 = load i32, i32* %t2079, align 4
  %t4468.not = icmp eq i32 %t4466, 0
  br i1 %t4468.not, label %L_280, label %L_331

L_329:                                            ; preds = %L_327, %L_337, %L_339
  %TW64_0.5 = phi i64 [ %t4392, %L_339 ], [ %t4423, %L_337 ], [ %t4505, %L_327 ]
  %t4472.not = icmp eq i64 %TW64_0.5, -1
  br i1 %t4472.not, label %L_333, label %L_330

L_327:                                            ; preds = %L_326
  %t4505 = tail call i64 @Posix_IO_writeChar8Vec(i32 1, i8* %t4499, i32 %t4483, i64 %t4501)
  br label %L_329

L_326:                                            ; preds = %L_324
  %t4510 = getelementptr inbounds i8, i8* %t4526, i64 -8
  %t4511 = bitcast i8* %t4510 to i64*
  %t4512 = load i64, i64* %t4511, align 4
  %t4514.mask = and i64 %t4512, -2
  %switch705 = icmp eq i64 %t4514.mask, 92
  %t4481 = getelementptr inbounds i8, i8* %t4526, i64 4
  %t4482 = bitcast i8* %t4481 to i32*
  %t4483 = load i32, i32* %t4482, align 4
  %t4490 = bitcast i8* %t4526 to i32*
  %t4491 = load i32, i32* %t4490, align 4
  %t4497 = getelementptr inbounds i8, i8* %t4526, i64 8
  %t4498 = bitcast i8* %t4497 to i8**
  %t4499 = load i8*, i8** %t4498, align 8
  %t4501 = sext i32 %t4491 to i64
  br i1 %switch705, label %L_327, label %L_337

L_324:                                            ; preds = %L_306
  store i32 1, i32* %t2079, align 4
  %t4526 = load i8*, i8** %t5104, align 8
  %t4527 = ptrtoint i8* %t4526 to i64
  %t4528 = and i64 %t4527, 1
  %cond1.not = icmp eq i64 %t4528, 0
  br i1 %cond1.not, label %L_326, label %L_339

L_322:                                            ; preds = %L_307
  %t4537 = lshr i64 %t4680, 1
  %t4539 = trunc i64 %t4537 to i32
  %t4541 = tail call i64 @Posix_IO_lseek(i32 %t4539, i64 0, i32 1)
  br label %L_312

L_320:                                            ; preds = %L_309
  %t4572 = tail call i64 @Posix_IO_writeChar8Arr(i32 1, i8* %t4652, i32 %t4636, i64 %t4654)
  br label %L_312

L_318:                                            ; preds = %L_316
  %t4576 = add i32 %t4587, -1
  store i32 %t4576, i32* %t2079, align 4
  br label %raiseSys_0

L_316:                                            ; preds = %L_312
  %t4592 = tail call i32 @Posix_Error_getErrno()
  %t4587 = load i32, i32* %t2079, align 4
  %t4589.not = icmp eq i32 %t4587, 0
  br i1 %t4589.not, label %L_280, label %L_318

L_314:                                            ; preds = %L_313
  %t4596 = add i32 %t4619, -1
  store i32 %t4596, i32* %t2079, align 4
  %t4602 = bitcast i8* %stackTop.46 to i64*
  store i64 %TW64_0.6, i64* %t4602, align 4
  %t4605 = getelementptr inbounds i8, i8* %stackTop.46, i64 -8
  %t4606 = bitcast i8* %t4605 to i64*
  %t4607 = load i64, i64* %t4606, align 4
  %t4608 = getelementptr inbounds [84 x i64 (i8*, i8*, i8*, i64)*], [84 x i64 (i8*, i8*, i8*, i64)*]* @nextChunks, i64 0, i64 %t4607
  %t4609 = load i64 (i8*, i8*, i8*, i64)*, i64 (i8*, i8*, i8*, i64)** %t4608, align 8
  %t4610 = icmp eq i64 (i8*, i8*, i8*, i64)* %t4609, @Chunk_11
  br i1 %t4610, label %doSwitchNextBlock.backedge, label %L_1964

L_1964:                                           ; preds = %L_314
  store i8* %frontier.46, i8** %t5312, align 8
  store i8* %stackTop.46, i8** %t5315, align 8
  br label %common.ret

L_313:                                            ; preds = %L_312
  %t4619 = load i32, i32* %t2079, align 4
  %t4621.not = icmp eq i32 %t4619, 0
  br i1 %t4621.not, label %L_280, label %L_314

L_312:                                            ; preds = %L_310, %L_320, %L_322
  %TW64_0.6 = phi i64 [ %t4541, %L_322 ], [ %t4572, %L_320 ], [ %t4658, %L_310 ]
  %t4625.not = icmp eq i64 %TW64_0.6, -1
  br i1 %t4625.not, label %L_316, label %L_313

L_310:                                            ; preds = %L_309
  %t4658 = tail call i64 @Posix_IO_writeChar8Vec(i32 1, i8* %t4652, i32 %t4636, i64 %t4654)
  br label %L_312

L_309:                                            ; preds = %L_307
  %t4663 = getelementptr inbounds i8, i8* %t4679, i64 -8
  %t4664 = bitcast i8* %t4663 to i64*
  %t4665 = load i64, i64* %t4664, align 4
  %t4667.mask = and i64 %t4665, -2
  %switch709 = icmp eq i64 %t4667.mask, 92
  %t4634 = getelementptr inbounds i8, i8* %t4679, i64 4
  %t4635 = bitcast i8* %t4634 to i32*
  %t4636 = load i32, i32* %t4635, align 4
  %t4643 = bitcast i8* %t4679 to i32*
  %t4644 = load i32, i32* %t4643, align 4
  %t4650 = getelementptr inbounds i8, i8* %t4679, i64 8
  %t4651 = bitcast i8* %t4650 to i8**
  %t4652 = load i8*, i8** %t4651, align 8
  %t4654 = sext i32 %t4644 to i64
  br i1 %switch709, label %L_310, label %L_320

L_307:                                            ; preds = %L_306
  %t4672 = add nuw i32 %t4685, 1
  store i32 %t4672, i32* %t2079, align 4
  %t4679 = load i8*, i8** %t5104, align 8
  %t4680 = ptrtoint i8* %t4679 to i64
  %t4681 = and i64 %t4680, 1
  %cond2.not = icmp eq i64 %t4681, 0
  br i1 %cond2.not, label %L_309, label %L_322

L_306:                                            ; preds = %L_283, %L_335
  %t4685 = phi i32 [ %t4427, %L_335 ], [ %t4969, %L_283 ]
  %t4687.not = icmp eq i32 %t4685, 0
  br i1 %t4687.not, label %L_324, label %L_307

L_286:                                            ; preds = %raiseSys_0
  %t4702 = bitcast i8* %t4974 to i64*
  store i64 152, i64* %t4702, align 4
  %t4704 = getelementptr inbounds i8, i8* %stackTop.46, i64 16
  store i8* %frontier.46, i8** %t5312, align 8
  store i8* %t4704, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4714 = load i8*, i8** %t5312, align 8
  %t4717 = load i8*, i8** %t5315, align 8
  %t4699 = getelementptr inbounds i8, i8* %t4717, i64 -16
  %t4952.phi.trans.insert = bitcast i8* %t4699 to i32*
  %t4953.pre = load i32, i32* %t4952.phi.trans.insert, align 4
  br label %L_288

L_301:                                            ; preds = %L_1813
  %t4722 = load i8*, i8** %t4075, align 8
  %t4725 = load i64, i64* %t4081, align 4
  %t4726 = getelementptr inbounds i8, i8* %t4722, i64 %t4725
  %t4729 = bitcast i8* %t4726 to i8**
  store i8* inttoptr (i64 2 to i8*), i8** %t4729, align 8
  %t4733 = load i8*, i8** %t4075, align 8
  %t4736 = load i64, i64* %t4081, align 4
  %t4737 = getelementptr inbounds i8, i8* %t4733, i64 %t4736
  %t4739 = getelementptr inbounds i8, i8* %t4737, i64 -8
  %t4740 = bitcast i8* %t4739 to i64*
  %t4741 = load i64, i64* %t4740, align 4
  %t4742 = getelementptr inbounds [84 x i64 (i8*, i8*, i8*, i64)*], [84 x i64 (i8*, i8*, i8*, i64)*]* @nextChunks, i64 0, i64 %t4741
  %t4743 = load i64 (i8*, i8*, i8*, i64)*, i64 (i8*, i8*, i8*, i64)** %t4742, align 8
  %t4744 = icmp eq i64 (i8*, i8*, i8*, i64)* %t4743, @Chunk_11
  br i1 %t4744, label %doSwitchNextBlock.backedge, label %L_1971

L_1971:                                           ; preds = %L_301
  store i8* %frontier.45, i8** %t5312, align 8
  store i8* %t4737, i8** %t5315, align 8
  br label %common.ret

L_300:                                            ; preds = %L_300.preheader1435, %L_300
  %TW64_1.2916 = phi i64 [ %t4772, %L_300 ], [ %TW64_1.2916.ph, %L_300.preheader1435 ]
  %40 = shl i64 %TW64_1.2916, 32
  %t4756 = ashr exact i64 %40, 32
  %t4760 = getelementptr inbounds i8, i8* %t4881.pre, i64 %t4756
  %t4763 = load i8, i8* %t4760, align 1
  %t4767 = getelementptr inbounds i8, i8* %t4900, i64 %TW64_1.2916
  store i8 %t4763, i8* %t4767, align 1
  %t4772 = add nuw nsw i64 %TW64_1.2916, 1
  %t4870.not = icmp slt i64 %t4772, %t4877.pre
  br i1 %t4870.not, label %L_300, label %L_1814, !llvm.loop !8

L_296:                                            ; preds = %L_1814
  %t4778 = bitcast i8* %t48791126 to i64*
  store i64 151, i64* %t4778, align 4
  %t4780 = getelementptr inbounds i8, i8* %stackTop.441123, i64 24
  store i8* %frontier.441124, i8** %t5312, align 8
  store i8* %t4780, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4790 = load i8*, i8** %t5312, align 8
  %t4793 = load i8*, i8** %t5315, align 8
  %t4775 = getelementptr inbounds i8, i8* %t4793, i64 -24
  %t4847.phi.trans.insert = getelementptr inbounds i8, i8* %t4793, i64 -16
  %t4848.phi.trans.insert = bitcast i8* %t4847.phi.trans.insert to i8**
  %t4849.pre = load i8*, i8** %t4848.phi.trans.insert, align 8
  br label %L_298

L_299:                                            ; preds = %L_288, %L_298
  %t4808.pre-phi = phi i32* [ %t4952.pre-phi, %L_288 ], [ %.pre, %L_298 ]
  %TP_0.12 = phi i8* [ getelementptr (i8, i8* @staticHeapI, i64 3272), %L_288 ], [ %t4855, %L_298 ]
  %frontier.42 = phi i8* [ %frontier.45, %L_288 ], [ %frontier.43, %L_298 ]
  %t4795 = getelementptr inbounds i8, i8* %frontier.42, i64 8
  %t4800 = bitcast i8* %frontier.42 to i64*
  store i64 125, i64* %t4800, align 4
  %t4802 = getelementptr inbounds i8, i8* %frontier.42, i64 24
  %t4805 = bitcast i8* %t4795 to i32*
  %t4809 = load i32, i32* %t4808.pre-phi, align 4
  store i32 %t4809, i32* %t4805, align 4
  %t4811 = getelementptr inbounds i8, i8* %frontier.42, i64 16
  %t4812 = bitcast i8* %t4811 to i8**
  store i8* %TP_0.12, i8** %t4812, align 8
  %t4816 = load i8*, i8** %t4075, align 8
  %t4819 = load i64, i64* %t4081, align 4
  %t4820 = getelementptr inbounds i8, i8* %t4816, i64 %t4819
  %t4823 = bitcast i8* %t4820 to i8**
  store i8* %t4795, i8** %t4823, align 8
  %t4828 = load i8*, i8** %t4075, align 8
  %t4831 = load i64, i64* %t4081, align 4
  %t4832 = getelementptr inbounds i8, i8* %t4828, i64 %t4831
  %t4834 = getelementptr inbounds i8, i8* %t4832, i64 -8
  %t4835 = bitcast i8* %t4834 to i64*
  %t4836 = load i64, i64* %t4835, align 4
  %t4837 = getelementptr inbounds [84 x i64 (i8*, i8*, i8*, i64)*], [84 x i64 (i8*, i8*, i8*, i64)*]* @nextChunks, i64 0, i64 %t4836
  %t4838 = load i64 (i8*, i8*, i8*, i64)*, i64 (i8*, i8*, i8*, i64)** %t4837, align 8
  %t4839 = icmp eq i64 (i8*, i8*, i8*, i64)* %t4838, @Chunk_11
  br i1 %t4839, label %doSwitchNextBlock.backedge, label %L_1973

L_1973:                                           ; preds = %L_299
  store i8* %t4802, i8** %t5312, align 8
  store i8* %t4832, i8** %t5315, align 8
  br label %common.ret

L_298:                                            ; preds = %L_1814, %L_296
  %t4848.pre-phi = phi i8** [ %t4859, %L_1814 ], [ %t4848.phi.trans.insert, %L_296 ]
  %t4849 = phi i8* [ %TP_0.131122, %L_1814 ], [ %t4849.pre, %L_296 ]
  %stackTop.43 = phi i8* [ %stackTop.441123, %L_1814 ], [ %t4775, %L_296 ]
  %frontier.43 = phi i8* [ %frontier.441124, %L_1814 ], [ %t4790, %L_296 ]
  %t4850 = getelementptr inbounds i8, i8* %t4849, i64 -8
  %t4851 = bitcast i8* %t4850 to i64*
  store i64 11, i64* %t4851, align 4
  %t4855 = load i8*, i8** %t4848.pre-phi, align 8
  %.pre = bitcast i8* %stackTop.43 to i32*
  br label %L_299

L_1814:                                           ; preds = %L_300, %middle.block, %L_join_3.thread, %L_join_3
  %t48791126 = phi i8* [ %t48791120, %L_join_3.thread ], [ %t4879, %L_join_3 ], [ %t4879, %middle.block ], [ %t4879, %L_300 ]
  %t48751125 = phi i8* [ %t48751119, %L_join_3.thread ], [ %t4875, %L_join_3 ], [ %t4875, %middle.block ], [ %t4875, %L_300 ]
  %frontier.441124 = phi i8* [ %frontier.45, %L_join_3.thread ], [ %t4903, %L_join_3 ], [ %t4903, %middle.block ], [ %t4903, %L_300 ]
  %stackTop.441123 = phi i8* [ %stackTop.45, %L_join_3.thread ], [ %t4883, %L_join_3 ], [ %t4883, %middle.block ], [ %t4883, %L_300 ]
  %TP_0.131122 = phi i8* [ getelementptr (i8, i8* @staticHeapM, i64 64), %L_join_3.thread ], [ %t4900, %L_join_3 ], [ %t4900, %middle.block ], [ %t4900, %L_300 ]
  %t4859 = bitcast i8* %t48751125 to i8**
  store i8* %TP_0.131122, i8** %t4859, align 8
  %t4863 = load i8*, i8** %t5411, align 8
  %t4865.not = icmp ult i8* %t4863, %frontier.441124
  br i1 %t4865.not, label %L_296, label %L_298

L_join_3:                                         ; preds = %L_293
  %t4890 = getelementptr inbounds i8, i8* %stackTop.45, i64 24
  %t4891 = bitcast i8* %t4890 to i64*
  store i64 94, i64* %t4891, align 4
  %t4893 = getelementptr inbounds i8, i8* %stackTop.45, i64 32
  store i8* %frontier.45, i8** %t5312, align 8
  store i8* %t4893, i8** %t5315, align 8
  %t4900 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t4933, i64 21)
  %t4903 = load i8*, i8** %t5312, align 8
  %t4906 = load i8*, i8** %t5315, align 8
  %t4883 = getelementptr inbounds i8, i8* %t4906, i64 -32
  %t4875.phi.trans.insert = getelementptr inbounds i8, i8* %t4906, i64 -24
  %t4876.phi.trans.insert = bitcast i8* %t4875.phi.trans.insert to i64*
  %t4877.pre = load i64, i64* %t4876.phi.trans.insert, align 4
  %t4879.phi.trans.insert = getelementptr inbounds i8, i8* %t4906, i64 -16
  %t4880.phi.trans.insert = bitcast i8* %t4879.phi.trans.insert to i8**
  %t4881.pre = load i8*, i8** %t4880.phi.trans.insert, align 8
  %t4875 = getelementptr inbounds i8, i8* %t4906, i64 -24
  %t4879 = getelementptr inbounds i8, i8* %t4906, i64 -16
  %t4870.not915 = icmp sgt i64 %t4877.pre, 0
  br i1 %t4870.not915, label %L_300.preheader, label %L_1814

L_300.preheader:                                  ; preds = %L_join_3
  %41 = add i64 %t4877.pre, -4
  %42 = icmp ult i64 %41, 2147483645
  br i1 %42, label %vector.memcheck, label %L_300.preheader1435

vector.memcheck:                                  ; preds = %L_300.preheader
  %scevgep = getelementptr i8, i8* %t4900, i64 %t4877.pre
  %scevgep1410 = getelementptr i8, i8* %t4881.pre, i64 %t4877.pre
  %bound0 = icmp ult i8* %t4900, %scevgep1410
  %bound1 = icmp ult i8* %t4881.pre, %scevgep
  %found.conflict = and i1 %bound0, %bound1
  br i1 %found.conflict, label %L_300.preheader1435, label %vector.ph

vector.ph:                                        ; preds = %vector.memcheck
  %n.vec = and i64 %t4877.pre, -4
  br label %vector.body

vector.body:                                      ; preds = %vector.body, %vector.ph
  %index = phi i64 [ 0, %vector.ph ], [ %index.next, %vector.body ]
  %43 = shl i64 %index, 32
  %44 = ashr exact i64 %43, 32
  %45 = getelementptr inbounds i8, i8* %t4881.pre, i64 %44
  %46 = bitcast i8* %45 to <4 x i8>*
  %wide.load = load <4 x i8>, <4 x i8>* %46, align 1, !alias.scope !9
  %47 = getelementptr inbounds i8, i8* %t4900, i64 %index
  %48 = bitcast i8* %47 to <4 x i8>*
  store <4 x i8> %wide.load, <4 x i8>* %48, align 1, !alias.scope !12, !noalias !9
  %index.next = add nuw i64 %index, 4
  %49 = icmp eq i64 %index.next, %n.vec
  br i1 %49, label %middle.block, label %vector.body, !llvm.loop !14

middle.block:                                     ; preds = %vector.body
  %cmp.n = icmp eq i64 %t4877.pre, %n.vec
  br i1 %cmp.n, label %L_1814, label %L_300.preheader1435

L_300.preheader1435:                              ; preds = %vector.memcheck, %L_300.preheader, %middle.block
  %TW64_1.2916.ph = phi i64 [ 0, %vector.memcheck ], [ 0, %L_300.preheader ], [ %n.vec, %middle.block ]
  br label %L_300

L_293:                                            ; preds = %L_1813
  %t4911.not = icmp eq i32 %TW32_0.6, 0
  br i1 %t4911.not, label %L_join_3.thread, label %L_join_3

L_join_3.thread:                                  ; preds = %L_293
  %t48751119 = getelementptr inbounds i8, i8* %stackTop.45, i64 8
  %t48791120 = getelementptr inbounds i8, i8* %stackTop.45, i64 16
  br label %L_1814

L_1813:                                           ; preds = %loop_11
  %t4915 = getelementptr inbounds i8, i8* %stackTop.45, i64 8
  %t4916 = bitcast i8* %t4915 to i64*
  store i64 %t4933, i64* %t4916, align 4
  %t4919 = getelementptr inbounds i8, i8* %stackTop.45, i64 16
  %t4920 = bitcast i8* %t4919 to i8**
  store i8* %t4945, i8** %t4920, align 8
  %trunc515 = icmp sgt i32 %TW32_0.6, -1
  br i1 %trunc515, label %L_293, label %L_301

loop_11:                                          ; preds = %L_288, %loop_11
  %TW32_0.6 = phi i32 [ %t4930, %loop_11 ], [ 0, %L_288 ]
  %t4933 = sext i32 %TW32_0.6 to i64
  %t4937 = getelementptr inbounds i8, i8* %t4945, i64 %t4933
  %t4940 = load i8, i8* %t4937, align 1
  %cond3 = icmp eq i8 %t4940, 0
  %t4930 = add i32 %TW32_0.6, 1
  br i1 %cond3, label %L_1813, label %loop_11

L_288:                                            ; preds = %raiseSys_0.L_288_crit_edge, %L_286
  %t4952.pre-phi = phi i32* [ %.pre1096, %raiseSys_0.L_288_crit_edge ], [ %t4952.phi.trans.insert, %L_286 ]
  %t4953 = phi i32 [ %t4985.sink, %raiseSys_0.L_288_crit_edge ], [ %t4953.pre, %L_286 ]
  %stackTop.45 = phi i8* [ %stackTop.46, %raiseSys_0.L_288_crit_edge ], [ %t4699, %L_286 ]
  %frontier.45 = phi i8* [ %frontier.46, %raiseSys_0.L_288_crit_edge ], [ %t4714, %L_286 ]
  %t4954 = tail call i64 @Posix_Error_strError(i32 %t4953)
  %t4945 = inttoptr i64 %t4954 to i8*
  %t4947.not = icmp eq i64 %t4954, 0
  br i1 %t4947.not, label %L_299, label %loop_11

raiseSys_0:                                       ; preds = %L_335, %L_283, %L_318
  %t4985.sink = phi i32 [ %t4592, %L_318 ], [ %t4985, %L_283 ], [ %t4439, %L_335 ]
  %t4964 = bitcast i8* %stackTop.46 to i32*
  store i32 %t4985.sink, i32* %t4964, align 4
  %t4957 = load i8*, i8** %t5411, align 8
  %t4959.not = icmp ult i8* %t4957, %frontier.46
  br i1 %t4959.not, label %L_286, label %raiseSys_0.L_288_crit_edge

raiseSys_0.L_288_crit_edge:                       ; preds = %raiseSys_0
  %.pre1096 = bitcast i8* %stackTop.46 to i32*
  br label %L_288

L_283:                                            ; preds = %L_281
  %t4969 = add i32 %t4980, -1
  store i32 %t4969, i32* %t2079, align 4
  %t4974 = getelementptr inbounds i8, i8* %stackTop.46, i64 8
  %t4975 = bitcast i8* %t4974 to i32*
  %t4976 = load i32, i32* %t4975, align 4
  %switch718 = icmp ne i32 %t4976, 0
  %t4695.not913 = icmp eq i32 %t4985, 4
  %or.cond1276 = select i1 %switch718, i1 %t4695.not913, i1 false
  br i1 %or.cond1276, label %L_306, label %raiseSys_0

L_281:                                            ; preds = %L_276
  %t4985 = tail call i32 @Posix_Error_getErrno()
  %t4980 = load i32, i32* %t2079, align 4
  %t4982.not = icmp eq i32 %t4980, 0
  br i1 %t4982.not, label %L_280, label %L_283

L_280:                                            ; preds = %L_333, %L_277, %L_281, %L_313, %L_316, %L_330
  %t4988 = load i8*, i8** %t4075, align 8
  %t4991 = load i64, i64* %t4081, align 4
  %t4992 = getelementptr inbounds i8, i8* %t4988, i64 %t4991
  %t4995 = bitcast i8* %t4992 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 8968), i8** %t4995, align 8
  %t5001 = load i8*, i8** %t4075, align 8
  %t5004 = load i64, i64* %t4081, align 4
  %t5005 = getelementptr inbounds i8, i8* %t5001, i64 %t5004
  %t5007 = getelementptr inbounds i8, i8* %t5005, i64 -8
  %t5008 = bitcast i8* %t5007 to i64*
  %t5009 = load i64, i64* %t5008, align 4
  %t5010 = getelementptr inbounds [84 x i64 (i8*, i8*, i8*, i64)*], [84 x i64 (i8*, i8*, i8*, i64)*]* @nextChunks, i64 0, i64 %t5009
  %t5011 = load i64 (i8*, i8*, i8*, i64)*, i64 (i8*, i8*, i8*, i64)** %t5010, align 8
  %t5012 = icmp eq i64 (i8*, i8*, i8*, i64)* %t5011, @Chunk_11
  br i1 %t5012, label %doSwitchNextBlock.backedge, label %L_1983

L_1983:                                           ; preds = %L_280
  store i8* %frontier.46, i8** %t5312, align 8
  store i8* %t5005, i8** %t5315, align 8
  br label %common.ret

L_278:                                            ; preds = %L_277
  %t5022 = add i32 %t5045, -1
  store i32 %t5022, i32* %t2079, align 4
  %t5028 = bitcast i8* %stackTop.46 to i64*
  store i64 %TW64_0.7, i64* %t5028, align 4
  %t5031 = getelementptr inbounds i8, i8* %stackTop.46, i64 -8
  %t5032 = bitcast i8* %t5031 to i64*
  %t5033 = load i64, i64* %t5032, align 4
  %t5034 = getelementptr inbounds [84 x i64 (i8*, i8*, i8*, i64)*], [84 x i64 (i8*, i8*, i8*, i64)*]* @nextChunks, i64 0, i64 %t5033
  %t5035 = load i64 (i8*, i8*, i8*, i64)*, i64 (i8*, i8*, i8*, i64)** %t5034, align 8
  %t5036 = icmp eq i64 (i8*, i8*, i8*, i64)* %t5035, @Chunk_11
  br i1 %t5036, label %doSwitchNextBlock.backedge, label %L_1985

L_1985:                                           ; preds = %L_278
  store i8* %frontier.46, i8** %t5312, align 8
  store i8* %stackTop.46, i8** %t5315, align 8
  br label %common.ret

L_277:                                            ; preds = %L_276
  %t5045 = load i32, i32* %t2079, align 4
  %t5047.not = icmp eq i32 %t5045, 0
  br i1 %t5047.not, label %L_280, label %L_278

L_276:                                            ; preds = %L_274, %L_341, %L_343
  %TW64_0.7 = phi i64 [ %t4349, %L_343 ], [ %t4380, %L_341 ], [ %t5084, %L_274 ]
  %t5051.not = icmp eq i64 %TW64_0.7, -1
  br i1 %t5051.not, label %L_281, label %L_277

L_274:                                            ; preds = %L_273
  %t5084 = tail call i64 @Posix_IO_writeChar8Vec(i32 1, i8* %t5078, i32 %t5062, i64 %t5080)
  br label %L_276

L_273:                                            ; preds = %L_271
  %t5089 = getelementptr inbounds i8, i8* %t5105, i64 -8
  %t5090 = bitcast i8* %t5089 to i64*
  %t5091 = load i64, i64* %t5090, align 4
  %t5093.mask = and i64 %t5091, -2
  %switch722 = icmp eq i64 %t5093.mask, 92
  %t5060 = getelementptr inbounds i8, i8* %t5105, i64 4
  %t5061 = bitcast i8* %t5060 to i32*
  %t5062 = load i32, i32* %t5061, align 4
  %t5069 = bitcast i8* %t5105 to i32*
  %t5070 = load i32, i32* %t5069, align 4
  %t5076 = getelementptr inbounds i8, i8* %t5105, i64 8
  %t5077 = bitcast i8* %t5076 to i8**
  %t5078 = load i8*, i8** %t5077, align 8
  %t5080 = sext i32 %t5070 to i64
  br i1 %switch722, label %L_274, label %L_341

L_271:                                            ; preds = %L_268, %L_269
  %stackTop.46 = phi i8* [ %t4319, %L_269 ], [ %stackTop.47, %L_268 ]
  %frontier.46 = phi i8* [ %t4334, %L_269 ], [ %frontier.47, %L_268 ]
  %t5097 = load i32, i32* %t2079, align 4
  %t5098 = add i32 %t5097, 1
  store i32 %t5098, i32* %t2079, align 4
  %t5104 = bitcast i8* %stackTop.46 to i8**
  %t5105 = load i8*, i8** %t5104, align 8
  %t5106 = ptrtoint i8* %t5105 to i64
  %t5107 = and i64 %t5106, 1
  %cond.not = icmp eq i64 %t5107, 0
  br i1 %cond.not, label %L_273, label %L_343

L_268.sink.split:                                 ; preds = %L_355, %L_25
  %t752.sink = phi i8* [ %t752, %L_25 ], [ %t1442, %L_355 ]
  %.sink1275 = phi i64 [ 15, %L_25 ], [ 18, %L_355 ]
  %stackTop.47.ph = phi i8* [ %t744, %L_25 ], [ %t1434, %L_355 ]
  %frontier.9.pn = phi i8* [ %frontier.9, %L_25 ], [ %frontier.17, %L_355 ]
  %frontier.47.ph = getelementptr inbounds i8, i8* %frontier.9.pn, i64 24
  %t753 = bitcast i8* %t752.sink to i64*
  store i64 %.sink1275, i64* %t753, align 4
  br label %L_268

L_268:                                            ; preds = %L_268.sink.split, %doSwitchNextBlock
  %stackTop.47 = phi i8* [ %stackTop.0, %doSwitchNextBlock ], [ %stackTop.47.ph, %L_268.sink.split ]
  %frontier.47 = phi i8* [ %frontier.0, %doSwitchNextBlock ], [ %frontier.47.ph, %L_268.sink.split ]
  %t5111 = load i8*, i8** %t4312, align 8
  %t5113.not = icmp ult i8* %t5111, %stackTop.47
  br i1 %t5113.not, label %L_269, label %L_271

L_1256:                                           ; preds = %L_1766
  %t5119 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t5120 = bitcast i8* %t5119 to i64*
  store i64 105, i64* %t5120, align 4
  store i8* %frontier.0, i8** %t5312, align 8
  store i8* %stackTop.0, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t5132 = load i8*, i8** %t5312, align 8
  %t5135 = load i8*, i8** %t5315, align 8
  br label %L_1258

L_1267:                                           ; preds = %L_1258
  %t5166 = getelementptr inbounds i8, i8* %t5135.pn, i64 -168
  %t5167 = bitcast i8* %t5166 to i64*
  store i64 60, i64* %t5167, align 4
  %t5170 = bitcast i8* %t5135.pn to i8**
  store i8* %t5212, i8** %t5170, align 8
  %t5173 = getelementptr inbounds i8, i8* %t5135.pn, i64 8
  %t5174 = bitcast i8* %t5173 to i8**
  store i8* %t5264, i8** %t5174, align 8
  %t5177 = getelementptr inbounds i8, i8* %t5135.pn, i64 -8
  %t5178 = bitcast i8* %t5177 to i64*
  store i64 33, i64* %t5178, align 4
  br label %L_345

L_1261:                                           ; preds = %doSwitchNextBlock, %doSwitchNextBlock
  %t5182 = getelementptr inbounds i8, i8* %stackTop.0, i64 -168
  %t5183 = bitcast i8* %t5182 to i64*
  store i64 6, i64* %t5183, align 4
  %t5185 = getelementptr inbounds i8, i8* %stackTop.0, i64 -144
  %t5186 = bitcast i8* %t5185 to i32*
  store i32 0, i32* %t5186, align 4
  %t5188 = getelementptr inbounds i8, i8* %stackTop.0, i64 -136
  %t5189 = bitcast i8* %t5188 to i8**
  %t5191 = getelementptr inbounds i8, i8* %stackTop.0, i64 -16
  %t5192 = bitcast i8* %t5191 to i8**
  %t5193 = load i8*, i8** %t5192, align 8
  store i8* %t5193, i8** %t5189, align 8
  %t5195 = getelementptr inbounds i8, i8* %stackTop.0, i64 -152
  br label %L_1821.sink.split

L_1259:                                           ; preds = %L_1258
  %t5230 = bitcast i8* %t5135.pn to i8**
  store i8* %t5212, i8** %t5230, align 8
  %t5233 = getelementptr inbounds i8, i8* %t5135.pn, i64 8
  %t5234 = bitcast i8* %t5233 to i8**
  store i8* %t5264, i8** %t5234, align 8
  %t5237 = getelementptr inbounds i8, i8* %t5135.pn, i64 -8
  %t5238 = bitcast i8* %t5237 to i64*
  store i64 34, i64* %t5238, align 4
  br label %L_345

L_1258:                                           ; preds = %L_1766, %L_1256
  %t5135.pn = phi i8* [ %t5135, %L_1256 ], [ %stackTop.0, %L_1766 ]
  %frontier.48 = phi i8* [ %t5132, %L_1256 ], [ %frontier.0, %L_1766 ]
  %t5242 = getelementptr inbounds i8, i8* %t5135.pn, i64 -152
  %t5243 = bitcast i8* %t5242 to i8**
  %t5245 = getelementptr inbounds i8, i8* %t5135.pn, i64 -144
  %50 = bitcast i8* %t5245 to i8***
  %t5247555 = load i8**, i8*** %50, align 8
  %t5250 = load i8*, i8** %t5247555, align 8
  store i8* %t5250, i8** %t5243, align 8
  %t5255 = getelementptr inbounds i8, i8* %t5250, i64 16
  %51 = bitcast i8* %t5255 to i8***
  %t5257556 = load i8**, i8*** %51, align 8
  %t5262 = getelementptr inbounds i8, i8* %t5250, i64 24
  %t5263 = bitcast i8* %t5262 to i8**
  %t5264 = load i8*, i8** %t5263, align 8
  %t5268 = load i8*, i8** %t5257556, align 8
  %t5270 = getelementptr inbounds i8, i8* %t5268, i64 -8
  %t5271 = bitcast i8* %t5270 to i64*
  %t5272 = load i64, i64* %t5271, align 4
  %t5274.mask = and i64 %t5272, -2
  %switch724 = icmp eq i64 %t5274.mask, 118
  %t5203 = getelementptr inbounds i8, i8* %t5268, i64 8
  %t5204 = bitcast i8* %t5203 to i8**
  %t5205 = load i8*, i8** %t5204, align 8
  %t5209 = bitcast i8* %t5268 to i8**
  %t5210 = load i8*, i8** %t5209, align 8
  %t5212 = getelementptr inbounds i8, i8* %frontier.48, i64 8
  %t5217 = bitcast i8* %frontier.48 to i64*
  store i64 79, i64* %t5217, align 4
  %t5219 = getelementptr inbounds i8, i8* %frontier.48, i64 24
  %t5222 = bitcast i8* %t5212 to i8**
  store i8* %t5205, i8** %t5222, align 8
  %t5225 = getelementptr inbounds i8, i8* %frontier.48, i64 16
  %t5226 = bitcast i8* %t5225 to i8**
  store i8* %t5210, i8** %t5226, align 8
  br i1 %switch724, label %L_1259, label %L_1267

L_1766:                                           ; preds = %doSwitchNextBlock
  %t5280 = load i8*, i8** %t5411, align 8
  %t5282.not = icmp ult i8* %t5280, %frontier.0
  br i1 %t5282.not, label %L_1256, label %L_1258

L_1765:                                           ; preds = %doSwitchNextBlock
  %t5289 = bitcast i8* %stackTop.0 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 3152), i8** %t5289, align 8
  %t5293 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  %t5294 = bitcast i8* %t5293 to i8**
  %t5296 = getelementptr inbounds i8, i8* %stackTop.0, i64 -144
  %t5297 = bitcast i8* %t5296 to i8**
  %t5298 = load i8*, i8** %t5297, align 8
  store i8* %t5298, i8** %t5294, align 8
  %t5300 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t5301 = bitcast i8* %t5300 to i64*
  store i64 35, i64* %t5301, align 4
  br label %L_1828

L_983:                                            ; preds = %L_1718
  %t5308 = bitcast i8* %stackTop.0 to i64*
  store i64 95, i64* %t5308, align 4
  %t5310 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  store i8* %frontier.0, i8** %t5312, align 8
  store i8* %t5310, i8** %t5315, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t5320 = load i8*, i8** %t5312, align 8
  %t5323 = load i8*, i8** %t5315, align 8
  %t5305 = getelementptr inbounds i8, i8* %t5323, i64 -48
  br label %L_985

print_5:                                          ; preds = %doSwitchNextBlock
  %t5346 = bitcast i8* %stackTop.0 to i8**
  %t5347 = load i8*, i8** %t5346, align 8
  tail call void @Stdio_print(i8* %t5347)
  %t5325 = getelementptr inbounds i8, i8* %stackTop.0, i64 -32
  %t5326 = bitcast i8* %t5325 to i64*
  store i64 9, i64* %t5326, align 4
  %t5328 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t5329 = bitcast i8* %t5328 to i32*
  store i32 1, i32* %t5329, align 4
  %t5334 = getelementptr inbounds i8, i8* %stackTop.0, i64 -16
  %t5335 = bitcast i8* %t5334 to i8**
  %t5336 = load i8*, i8** %t5335, align 8
  store i8* %t5336, i8** %t5346, align 8
  br label %L_1821.sink.split

L_985:                                            ; preds = %L_1718, %L_983
  %stackTop.50 = phi i8* [ %t5305, %L_983 ], [ %t5402, %L_1718 ]
  %frontier.49 = phi i8* [ %t5320, %L_983 ], [ %frontier.0, %L_1718 ]
  %t5351 = getelementptr inbounds i8, i8* %frontier.49, i64 8
  %t5356 = bitcast i8* %frontier.49 to i64*
  store i64 123, i64* %t5356, align 4
  %t5361 = bitcast i8* %t5351 to i8**
  %t5363 = getelementptr inbounds i8, i8* %stackTop.50, i64 32
  %t5364 = bitcast i8* %t5363 to i8**
  %t5365 = load i8*, i8** %t5364, align 8
  store i8* %t5365, i8** %t5361, align 8
  %t5367 = getelementptr inbounds i8, i8* %frontier.49, i64 16
  %t5368 = bitcast i8* %t5367 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 9144), i8** %t5368, align 8
  %t5372 = getelementptr inbounds i8, i8* %frontier.49, i64 32
  %t5376 = getelementptr inbounds i8, i8* %frontier.49, i64 24
  %t5377 = bitcast i8* %t5376 to i64*
  store i64 123, i64* %t5377, align 4
  %t5379 = getelementptr inbounds i8, i8* %frontier.49, i64 48
  %t5382 = bitcast i8* %t5372 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 3904), i8** %t5382, align 8
  %t5386 = getelementptr inbounds i8, i8* %frontier.49, i64 40
  %t5387 = bitcast i8* %t5386 to i8**
  store i8* %t5351, i8** %t5387, align 8
  %t5390 = getelementptr inbounds i8, i8* %stackTop.50, i64 8
  %t5391 = bitcast i8* %t5390 to i64*
  store i64 8, i64* %t5391, align 4
  %t5393 = getelementptr inbounds i8, i8* %stackTop.50, i64 40
  %t5394 = bitcast i8* %t5393 to i8**
  store i8* %t5372, i8** %t5394, align 8
  br label %L_511.sink.split

L_1718:                                           ; preds = %doSwitchNextBlock
  %t5402 = getelementptr inbounds i8, i8* %stackTop.0, i64 -40
  %t5404 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t5405 = bitcast i8* %t5404 to i8**
  %t5408 = bitcast i8* %stackTop.0 to i8**
  %t5409 = load i8*, i8** %t5408, align 8
  store i8* %t5409, i8** %t5405, align 8
  %t5412 = load i8*, i8** %t5411, align 8
  %t5414.not = icmp ult i8* %t5412, %frontier.0
  br i1 %t5414.not, label %L_983, label %L_985
}

define hidden i64 @Chunk_1(i8* %gcState, i8* %stackTopArg, i8* %frontierArg, i64 %nextBlockArg) local_unnamed_addr {
start:
  %t15209 = getelementptr inbounds i8, i8* %gcState, i64 24
  %t15210 = bitcast i8* %t15209 to i8**
  %t15197 = bitcast i8* %gcState to i8**
  %t15199 = getelementptr inbounds i8, i8* %gcState, i64 16
  %t15200 = bitcast i8* %t15199 to i8**
  %t14202 = getelementptr inbounds i8, i8* %gcState, i64 8
  %t14203 = bitcast i8* %t14202 to i8**
  %t14170 = getelementptr inbounds i8, i8* %gcState, i64 936
  %t14171 = bitcast i8* %t14170 to i8**
  %t13968 = getelementptr inbounds i8, i8* %gcState, i64 68
  %t13969 = bitcast i8* %t13968 to i32*
  %t8217 = getelementptr inbounds i8, i8* %gcState, i64 1064
  %t8218 = bitcast i8* %t8217 to i8**
  %t15179 = getelementptr inbounds i8, i8* %gcState, i64 32
  %t15180 = bitcast i8* %t15179 to i64*
  %t14422 = getelementptr inbounds i8, i8* %gcState, i64 1560
  %t14423 = bitcast i8* %t14422 to i8**
  br label %doSwitchNextBlock

doSwitchNextBlock:                                ; preds = %doSwitchNextBlock.backedge, %start
  %stackTop.0 = phi i8* [ %stackTopArg, %start ], [ %stackTop.1.sink, %doSwitchNextBlock.backedge ]
  %frontier.0 = phi i8* [ %frontierArg, %start ], [ %frontier.0.be, %doSwitchNextBlock.backedge ]
  %nextBlock.0 = phi i64 [ %nextBlockArg, %start ], [ %nextBlock.0.be, %doSwitchNextBlock.backedge ]
  switch i64 %nextBlock.0, label %switchNextBlockDefault [
    i64 39, label %L_81
    i64 40, label %L_120
    i64 41, label %L_142
    i64 42, label %L_213
    i64 43, label %L_249
    i64 44, label %L_219
    i64 45, label %L_597
    i64 46, label %L_638
    i64 47, label %L_749
    i64 48, label %L_1682
    i64 49, label %L_866
    i64 50, label %L_914
    i64 51, label %L_1592
    i64 52, label %L_941
    i64 53, label %L_1493
    i64 54, label %L_1493
    i64 55, label %L_1782
    i64 56, label %fromInt32Unsafe_3
    i64 57, label %L_1777
    i64 58, label %fromInt32Unsafe_2
    i64 59, label %L_1772
    i64 60, label %L_1269
    i64 61, label %L_1266
    i64 62, label %L_1764
    i64 63, label %L_1760
    i64 64, label %L_1759
    i64 65, label %L_1757
    i64 66, label %L_1746
    i64 67, label %L_1735
    i64 68, label %L_1712
    i64 69, label %L_1711
    i64 70, label %L_1710
    i64 71, label %L_1709
    i64 72, label %L_1708
    i64 73, label %L_1707
    i64 74, label %L_1706
    i64 75, label %L_1702
    i64 76, label %L_1701
    i64 77, label %L_1700
    i64 78, label %L_1699
    i64 79, label %L_1697
    i64 80, label %L_1696
    i64 81, label %L_1695
    i64 82, label %L_1694
    i64 83, label %L_0
  ]

switchNextBlockDefault:                           ; preds = %doSwitchNextBlock
  unreachable

L_82:                                             ; preds = %L_81
  %t4 = getelementptr inbounds i8, i8* %stackTop.2, i64 32
  %t5 = bitcast i8* %t4 to i64*
  store i64 177, i64* %t5, align 4
  %t7 = getelementptr inbounds i8, i8* %stackTop.2, i64 40
  store i8* %frontier.2, i8** %t15197, align 8
  store i8* %t7, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t17 = load i8*, i8** %t15197, align 8
  %t20 = load i8*, i8** %t15200, align 8
  %t2 = getelementptr inbounds i8, i8* %t20, i64 -40
  br label %L_84

L_116:                                            ; preds = %L_115
  %t42 = getelementptr inbounds i8, i8* %stackTop.1, i64 16
  %0 = bitcast i8* %t42 to i32**
  %t442009 = load i32*, i32** %0, align 8
  %t47 = load i32, i32* %t442009, align 4
  %t47.frozen = freeze i32 %t47
  %t53 = udiv i32 %t47.frozen, 44488
  %1 = mul i32 %t53, 44488
  %t49.decomposed = sub i32 %t47.frozen, %1
  %t51 = mul nuw nsw i32 %t49.decomposed, 48271
  %t55 = mul nuw nsw i32 %t53, 3399
  %t58.not = icmp ugt i32 %t51, %t55
  %t36 = xor i32 %t55, 2147483647
  %2 = sub nsw i32 0, %t55
  %TW32_0.0.p = select i1 %t58.not, i32 %2, i32 %t36
  %TW32_0.0 = add i32 %TW32_0.0.p, %t51
  store i32 %TW32_0.0, i32* %t442009, align 4
  br label %doSwitchNextBlock.backedge

doSwitchNextBlock.backedge:                       ; preds = %L_618, %L_624, %L_116, %L_90, %L_137, %L_133, %L_126, %L_207, %L_174, %L_236, %L_632, %L_629, %L_626, %L_609, %L_748, %L_697, %L_686, %L_653, %L_756
  %stackTop.1.sink = phi i8* [ %stackTop.1, %L_116 ], [ %stackTop.1, %L_90 ], [ %stackTop.3, %L_137 ], [ %stackTop.3, %L_133 ], [ %stackTop.3, %L_126 ], [ %stackTop.5, %L_207 ], [ %stackTop.64506, %L_174 ], [ %stackTop.13, %L_236 ], [ %t2029, %L_632 ], [ %t2052, %L_629 ], [ %t2075, %L_626 ], [ %t2248, %L_609 ], [ %t2380, %L_748 ], [ %stackTop.27, %L_697 ], [ %stackTop.28, %L_686 ], [ %t3452, %L_653 ], [ %stackTop.43, %L_756 ], [ %stackTop.18, %L_624 ], [ %stackTop.18, %L_618 ]
  %frontier.0.be = phi i8* [ %frontier.1, %L_116 ], [ %frontier.1, %L_90 ], [ %frontier.3, %L_137 ], [ %frontier.3, %L_133 ], [ %frontier.3, %L_126 ], [ %t774, %L_207 ], [ %frontier.64507, %L_174 ], [ %frontier.15, %L_236 ], [ %frontier.20, %L_632 ], [ %frontier.20, %L_629 ], [ %frontier.20, %L_626 ], [ %frontier.23, %L_609 ], [ %t3564, %L_748 ], [ %t2903, %L_697 ], [ %frontier.30, %L_686 ], [ %frontier.40, %L_653 ], [ %t3803, %L_756 ], [ %frontier.20, %L_624 ], [ %frontier.20, %L_618 ]
  %t32 = getelementptr inbounds i8, i8* %stackTop.1.sink, i64 -8
  %nextBlock.0.be.in = bitcast i8* %t32 to i64*
  %nextBlock.0.be = load i64, i64* %nextBlock.0.be.in, align 4
  br label %doSwitchNextBlock

L_115:                                            ; preds = %L_111
  %t642006 = load i32*, i32** %12, align 8
  store i32 %TW32_0.1, i32* %t642006, align 4
  %t712007 = load i32*, i32** %16, align 8
  store i32 1, i32* %t712007, align 4
  %t77 = load i8*, i8** %t239, align 8
  %t78 = getelementptr inbounds i8, i8* %t77, i64 -16
  %t79 = bitcast i8* %t78 to i64*
  %t80 = load i64, i64* %t79, align 4
  %t82.not = icmp eq i64 %t80, 0
  br i1 %t82.not, label %L_1266.sink.split, label %L_116

L_112:                                            ; preds = %L_109
  %3 = xor i32 %t126, -1
  %t91 = add i32 %t137, %3
  %t94.not = icmp ule i32 %t137, %t126
  br label %L_111

L_111:                                            ; preds = %L_112, %L_110
  %TW32_0.1.in = phi i1 [ %t113, %L_110 ], [ %t94.not, %L_112 ]
  %TW32_1.0 = phi i32 [ %t110, %L_110 ], [ %t91, %L_112 ]
  %TW32_0.1 = zext i1 %TW32_0.1.in to i32
  store i32 %TW32_1.0, i32* %t125, align 4
  %t174.not = icmp eq i32 %t167, 48
  br i1 %t174.not, label %L_115, label %L_107

L_110:                                            ; preds = %L_109
  %t110 = sub i32 %t137, %t126
  %t113 = icmp ult i32 %t137, %t126
  br label %L_111

L_109:                                            ; preds = %L_108
  %t122 = shl nsw i64 %t139, 2
  %t123 = getelementptr inbounds i8, i8* %t148, i64 %t122
  %t125 = bitcast i8* %t123 to i32*
  %t126 = load i32, i32* %t125, align 4
  %switch2145 = icmp eq i32 %TW32_0.23631, 0
  br i1 %switch2145, label %L_110, label %L_112

L_108:                                            ; preds = %L_107
  %t133 = shl nsw i64 %t153, 2
  %t134 = getelementptr inbounds i8, i8* %t148, i64 %t133
  %t136 = bitcast i8* %t134 to i32*
  %t137 = load i32, i32* %t136, align 4
  %t139 = sext i32 %TW32_1.13632 to i64
  %t142.not = icmp ugt i64 %t151, %t139
  br i1 %t142.not, label %L_109, label %L_1266.sink.split

L_107:                                            ; preds = %L_111, %L_100.7
  %TW32_1.13632 = phi i32 [ 8, %L_100.7 ], [ %t167, %L_111 ]
  %TW32_0.23631 = phi i32 [ %TW32_0.3.7, %L_100.7 ], [ %TW32_0.1, %L_111 ]
  %t167 = add nuw i32 %TW32_1.13632, 1
  %t160 = add i32 %TW32_1.13632, -8
  %t148 = load i8*, i8** %t239, align 8
  %t149 = getelementptr inbounds i8, i8* %t148, i64 -16
  %t150 = bitcast i8* %t149 to i64*
  %t151 = load i64, i64* %t150, align 4
  %t153 = sext i32 %t160 to i64
  %t156.not = icmp ugt i64 %t151, %t153
  br i1 %t156.not, label %L_108, label %L_1266.sink.split

L_101:                                            ; preds = %L_98
  %4 = xor i32 %t218, -1
  %t183 = add i32 %t229, %4
  %t186.not = icmp ule i32 %t229, %t218
  br label %L_96.1

L_96.1:                                           ; preds = %L_99, %L_101
  %TW32_0.3.in = phi i1 [ %t205, %L_99 ], [ %t186.not, %L_101 ]
  %TW32_1.2 = phi i32 [ %t202, %L_99 ], [ %t183, %L_101 ]
  store i32 %TW32_1.2, i32* %t217, align 4
  %t240.1 = load i8*, i8** %t239, align 8
  %t241.1 = getelementptr inbounds i8, i8* %t240.1, i64 -16
  %t242.1 = bitcast i8* %t241.1 to i64*
  %t243.1 = load i64, i64* %t242.1, align 4
  %t248.not.1 = icmp ugt i64 %t243.1, 41
  br i1 %t248.not.1, label %L_98.1, label %L_1266.sink.split

L_98.1:                                           ; preds = %L_96.1
  %t226.1 = getelementptr inbounds i8, i8* %t240.1, i64 164
  %t228.1 = bitcast i8* %t226.1 to i32*
  %t229.1 = load i32, i32* %t228.1, align 4
  %t215.1 = getelementptr inbounds i8, i8* %t240.1, i64 4
  %t217.1 = bitcast i8* %t215.1 to i32*
  %t218.1 = load i32, i32* %t217.1, align 4
  br i1 %TW32_0.3.in, label %L_101.1, label %L_99.1

L_101.1:                                          ; preds = %L_98.1
  %5 = xor i32 %t218.1, -1
  %t183.1 = add i32 %t229.1, %5
  %t186.not.1 = icmp ule i32 %t229.1, %t218.1
  br label %L_96.2

L_99.1:                                           ; preds = %L_98.1
  %t202.1 = sub i32 %t229.1, %t218.1
  %t205.1 = icmp ult i32 %t229.1, %t218.1
  br label %L_96.2

L_96.2:                                           ; preds = %L_101.1, %L_99.1
  %TW32_0.3.in.1 = phi i1 [ %t205.1, %L_99.1 ], [ %t186.not.1, %L_101.1 ]
  %TW32_1.2.1 = phi i32 [ %t202.1, %L_99.1 ], [ %t183.1, %L_101.1 ]
  store i32 %TW32_1.2.1, i32* %t217.1, align 4
  %t240.2 = load i8*, i8** %t239, align 8
  %t241.2 = getelementptr inbounds i8, i8* %t240.2, i64 -16
  %t242.2 = bitcast i8* %t241.2 to i64*
  %t243.2 = load i64, i64* %t242.2, align 4
  %t248.not.2 = icmp ugt i64 %t243.2, 42
  br i1 %t248.not.2, label %L_98.2, label %L_1266.sink.split

L_98.2:                                           ; preds = %L_96.2
  %t226.2 = getelementptr inbounds i8, i8* %t240.2, i64 168
  %t228.2 = bitcast i8* %t226.2 to i32*
  %t229.2 = load i32, i32* %t228.2, align 4
  %t215.2 = getelementptr inbounds i8, i8* %t240.2, i64 8
  %t217.2 = bitcast i8* %t215.2 to i32*
  %t218.2 = load i32, i32* %t217.2, align 4
  br i1 %TW32_0.3.in.1, label %L_101.2, label %L_99.2

L_101.2:                                          ; preds = %L_98.2
  %6 = xor i32 %t218.2, -1
  %t183.2 = add i32 %t229.2, %6
  %t186.not.2 = icmp ule i32 %t229.2, %t218.2
  br label %L_96.3

L_99.2:                                           ; preds = %L_98.2
  %t202.2 = sub i32 %t229.2, %t218.2
  %t205.2 = icmp ult i32 %t229.2, %t218.2
  br label %L_96.3

L_96.3:                                           ; preds = %L_101.2, %L_99.2
  %TW32_0.3.in.2 = phi i1 [ %t205.2, %L_99.2 ], [ %t186.not.2, %L_101.2 ]
  %TW32_1.2.2 = phi i32 [ %t202.2, %L_99.2 ], [ %t183.2, %L_101.2 ]
  store i32 %TW32_1.2.2, i32* %t217.2, align 4
  %t240.3 = load i8*, i8** %t239, align 8
  %t241.3 = getelementptr inbounds i8, i8* %t240.3, i64 -16
  %t242.3 = bitcast i8* %t241.3 to i64*
  %t243.3 = load i64, i64* %t242.3, align 4
  %t248.not.3 = icmp ugt i64 %t243.3, 43
  br i1 %t248.not.3, label %L_98.3, label %L_1266.sink.split

L_98.3:                                           ; preds = %L_96.3
  %t226.3 = getelementptr inbounds i8, i8* %t240.3, i64 172
  %t228.3 = bitcast i8* %t226.3 to i32*
  %t229.3 = load i32, i32* %t228.3, align 4
  %t215.3 = getelementptr inbounds i8, i8* %t240.3, i64 12
  %t217.3 = bitcast i8* %t215.3 to i32*
  %t218.3 = load i32, i32* %t217.3, align 4
  br i1 %TW32_0.3.in.2, label %L_101.3, label %L_99.3

L_101.3:                                          ; preds = %L_98.3
  %7 = xor i32 %t218.3, -1
  %t183.3 = add i32 %t229.3, %7
  %t186.not.3 = icmp ule i32 %t229.3, %t218.3
  br label %L_96.4

L_99.3:                                           ; preds = %L_98.3
  %t202.3 = sub i32 %t229.3, %t218.3
  %t205.3 = icmp ult i32 %t229.3, %t218.3
  br label %L_96.4

L_96.4:                                           ; preds = %L_101.3, %L_99.3
  %TW32_0.3.in.3 = phi i1 [ %t205.3, %L_99.3 ], [ %t186.not.3, %L_101.3 ]
  %TW32_1.2.3 = phi i32 [ %t202.3, %L_99.3 ], [ %t183.3, %L_101.3 ]
  store i32 %TW32_1.2.3, i32* %t217.3, align 4
  %t240.4 = load i8*, i8** %t239, align 8
  %t241.4 = getelementptr inbounds i8, i8* %t240.4, i64 -16
  %t242.4 = bitcast i8* %t241.4 to i64*
  %t243.4 = load i64, i64* %t242.4, align 4
  %t248.not.4 = icmp ugt i64 %t243.4, 44
  br i1 %t248.not.4, label %L_98.4, label %L_1266.sink.split

L_98.4:                                           ; preds = %L_96.4
  %t226.4 = getelementptr inbounds i8, i8* %t240.4, i64 176
  %t228.4 = bitcast i8* %t226.4 to i32*
  %t229.4 = load i32, i32* %t228.4, align 4
  %t215.4 = getelementptr inbounds i8, i8* %t240.4, i64 16
  %t217.4 = bitcast i8* %t215.4 to i32*
  %t218.4 = load i32, i32* %t217.4, align 4
  br i1 %TW32_0.3.in.3, label %L_101.4, label %L_99.4

L_101.4:                                          ; preds = %L_98.4
  %8 = xor i32 %t218.4, -1
  %t183.4 = add i32 %t229.4, %8
  %t186.not.4 = icmp ule i32 %t229.4, %t218.4
  br label %L_96.5

L_99.4:                                           ; preds = %L_98.4
  %t202.4 = sub i32 %t229.4, %t218.4
  %t205.4 = icmp ult i32 %t229.4, %t218.4
  br label %L_96.5

L_96.5:                                           ; preds = %L_101.4, %L_99.4
  %TW32_0.3.in.4 = phi i1 [ %t205.4, %L_99.4 ], [ %t186.not.4, %L_101.4 ]
  %TW32_1.2.4 = phi i32 [ %t202.4, %L_99.4 ], [ %t183.4, %L_101.4 ]
  store i32 %TW32_1.2.4, i32* %t217.4, align 4
  %t240.5 = load i8*, i8** %t239, align 8
  %t241.5 = getelementptr inbounds i8, i8* %t240.5, i64 -16
  %t242.5 = bitcast i8* %t241.5 to i64*
  %t243.5 = load i64, i64* %t242.5, align 4
  %t248.not.5 = icmp ugt i64 %t243.5, 45
  br i1 %t248.not.5, label %L_98.5, label %L_1266.sink.split

L_98.5:                                           ; preds = %L_96.5
  %t226.5 = getelementptr inbounds i8, i8* %t240.5, i64 180
  %t228.5 = bitcast i8* %t226.5 to i32*
  %t229.5 = load i32, i32* %t228.5, align 4
  %t215.5 = getelementptr inbounds i8, i8* %t240.5, i64 20
  %t217.5 = bitcast i8* %t215.5 to i32*
  %t218.5 = load i32, i32* %t217.5, align 4
  br i1 %TW32_0.3.in.4, label %L_101.5, label %L_99.5

L_101.5:                                          ; preds = %L_98.5
  %9 = xor i32 %t218.5, -1
  %t183.5 = add i32 %t229.5, %9
  %t186.not.5 = icmp ule i32 %t229.5, %t218.5
  br label %L_96.6

L_99.5:                                           ; preds = %L_98.5
  %t202.5 = sub i32 %t229.5, %t218.5
  %t205.5 = icmp ult i32 %t229.5, %t218.5
  br label %L_96.6

L_96.6:                                           ; preds = %L_101.5, %L_99.5
  %TW32_0.3.in.5 = phi i1 [ %t205.5, %L_99.5 ], [ %t186.not.5, %L_101.5 ]
  %TW32_1.2.5 = phi i32 [ %t202.5, %L_99.5 ], [ %t183.5, %L_101.5 ]
  store i32 %TW32_1.2.5, i32* %t217.5, align 4
  %t240.6 = load i8*, i8** %t239, align 8
  %t241.6 = getelementptr inbounds i8, i8* %t240.6, i64 -16
  %t242.6 = bitcast i8* %t241.6 to i64*
  %t243.6 = load i64, i64* %t242.6, align 4
  %t248.not.6 = icmp ugt i64 %t243.6, 46
  br i1 %t248.not.6, label %L_98.6, label %L_1266.sink.split

L_98.6:                                           ; preds = %L_96.6
  %t226.6 = getelementptr inbounds i8, i8* %t240.6, i64 184
  %t228.6 = bitcast i8* %t226.6 to i32*
  %t229.6 = load i32, i32* %t228.6, align 4
  %t215.6 = getelementptr inbounds i8, i8* %t240.6, i64 24
  %t217.6 = bitcast i8* %t215.6 to i32*
  %t218.6 = load i32, i32* %t217.6, align 4
  br i1 %TW32_0.3.in.5, label %L_101.6, label %L_99.6

L_101.6:                                          ; preds = %L_98.6
  %10 = xor i32 %t218.6, -1
  %t183.6 = add i32 %t229.6, %10
  %t186.not.6 = icmp ule i32 %t229.6, %t218.6
  br label %L_96.7

L_99.6:                                           ; preds = %L_98.6
  %t202.6 = sub i32 %t229.6, %t218.6
  %t205.6 = icmp ult i32 %t229.6, %t218.6
  br label %L_96.7

L_96.7:                                           ; preds = %L_101.6, %L_99.6
  %TW32_0.3.in.6 = phi i1 [ %t205.6, %L_99.6 ], [ %t186.not.6, %L_101.6 ]
  %TW32_1.2.6 = phi i32 [ %t202.6, %L_99.6 ], [ %t183.6, %L_101.6 ]
  store i32 %TW32_1.2.6, i32* %t217.6, align 4
  %t240.7 = load i8*, i8** %t239, align 8
  %t241.7 = getelementptr inbounds i8, i8* %t240.7, i64 -16
  %t242.7 = bitcast i8* %t241.7 to i64*
  %t243.7 = load i64, i64* %t242.7, align 4
  %t248.not.7 = icmp ugt i64 %t243.7, 47
  br i1 %t248.not.7, label %L_98.7, label %L_1266.sink.split

L_98.7:                                           ; preds = %L_96.7
  %t226.7 = getelementptr inbounds i8, i8* %t240.7, i64 188
  %t228.7 = bitcast i8* %t226.7 to i32*
  %t229.7 = load i32, i32* %t228.7, align 4
  %t215.7 = getelementptr inbounds i8, i8* %t240.7, i64 28
  %t217.7 = bitcast i8* %t215.7 to i32*
  %t218.7 = load i32, i32* %t217.7, align 4
  br i1 %TW32_0.3.in.6, label %L_101.7, label %L_99.7

L_101.7:                                          ; preds = %L_98.7
  %11 = xor i32 %t218.7, -1
  %t183.7 = add i32 %t229.7, %11
  %t186.not.7 = icmp ule i32 %t229.7, %t218.7
  br label %L_100.7

L_99.7:                                           ; preds = %L_98.7
  %t202.7 = sub i32 %t229.7, %t218.7
  %t205.7 = icmp ult i32 %t229.7, %t218.7
  br label %L_100.7

L_100.7:                                          ; preds = %L_99.7, %L_101.7
  %TW32_0.3.in.7 = phi i1 [ %t205.7, %L_99.7 ], [ %t186.not.7, %L_101.7 ]
  %TW32_1.2.7 = phi i32 [ %t202.7, %L_99.7 ], [ %t183.7, %L_101.7 ]
  %TW32_0.3.7 = zext i1 %TW32_0.3.in.7 to i32
  store i32 %TW32_1.2.7, i32* %t217.7, align 4
  br label %L_107

L_99:                                             ; preds = %L_98
  %t202 = sub i32 %t229, %t218
  %t205 = icmp ult i32 %t229, %t218
  br label %L_96.1

L_98:                                             ; preds = %L_96
  %t2722003 = load i32*, i32** %12, align 8
  %t275 = load i32, i32* %t2722003, align 4
  %t226 = getelementptr inbounds i8, i8* %t240, i64 160
  %t228 = bitcast i8* %t226 to i32*
  %t229 = load i32, i32* %t228, align 4
  %t217 = bitcast i8* %t240 to i32*
  %t218 = load i32, i32* %t217, align 4
  %switch2153 = icmp eq i32 %t275, 0
  br i1 %switch2153, label %L_99, label %L_101

L_96:                                             ; preds = %L_84
  %t270 = getelementptr inbounds i8, i8* %stackTop.1, i64 24
  %12 = bitcast i8* %t270 to i32**
  %t239 = bitcast i8* %stackTop.1 to i8**
  %t240 = load i8*, i8** %t239, align 8
  %t241 = getelementptr inbounds i8, i8* %t240, i64 -16
  %t242 = bitcast i8* %t241 to i64*
  %t243 = load i64, i64* %t242, align 4
  %t248.not = icmp ugt i64 %t243, 40
  br i1 %t248.not, label %L_98, label %L_1266.sink.split

L_90:                                             ; preds = %L_87
  %t316 = add i32 %t386, 1
  %t3002027 = load i32*, i32** %16, align 8
  store i32 %t316, i32* %t3002027, align 4
  br label %doSwitchNextBlock.backedge

L_87:                                             ; preds = %L_85
  %t329 = getelementptr inbounds i8, i8* %stackTop.1, i64 16
  %13 = bitcast i8* %t329 to i32**
  %t3312023 = load i32*, i32** %13, align 8
  %t334 = load i32, i32* %t3312023, align 4
  %t334.frozen = freeze i32 %t334
  %t340 = udiv i32 %t334.frozen, 44488
  %14 = mul i32 %t340, 44488
  %t336.decomposed = sub i32 %t334.frozen, %14
  %t338 = mul nuw nsw i32 %t336.decomposed, 48271
  %t342 = mul nuw nsw i32 %t340, 3399
  %t345.not = icmp ugt i32 %t338, %t342
  %t323 = xor i32 %t342, 2147483647
  %15 = sub nsw i32 0, %t342
  %TW32_1.4.p = select i1 %t345.not, i32 %15, i32 %t323
  %TW32_1.4 = add i32 %TW32_1.4.p, %t338
  store i32 %TW32_1.4, i32* %t3312023, align 4
  %t319.not = icmp eq i32 %t386, 2147483647
  br i1 %t319.not, label %L_1266.sink.split, label %L_90

L_85:                                             ; preds = %L_84
  %t368 = bitcast i8* %stackTop.1 to i8**
  %t369 = load i8*, i8** %t368, align 8
  %t370 = getelementptr inbounds i8, i8* %t369, i64 -16
  %t371 = bitcast i8* %t370 to i64*
  %t372 = load i64, i64* %t371, align 4
  %t374 = sext i32 %t386 to i64
  %t377.not = icmp ugt i64 %t372, %t374
  br i1 %t377.not, label %L_87, label %L_1266.sink.split

L_84:                                             ; preds = %L_81, %L_82
  %stackTop.1 = phi i8* [ %t2, %L_82 ], [ %stackTop.2, %L_81 ]
  %frontier.1 = phi i8* [ %t17, %L_82 ], [ %frontier.2, %L_81 ]
  %t381 = getelementptr inbounds i8, i8* %stackTop.1, i64 8
  %16 = bitcast i8* %t381 to i32**
  %t3832001 = load i32*, i32** %16, align 8
  %t386 = load i32, i32* %t3832001, align 4
  %t388.not = icmp eq i32 %t386, 48
  br i1 %t388.not, label %L_96, label %L_85

L_81.sink.split:                                  ; preds = %L_1776, %fromInt32Unsafe_2, %L_1781, %fromInt32Unsafe_3
  %t6717.sink = phi i8* [ %t6717, %fromInt32Unsafe_3 ], [ %t6772, %L_1781 ], [ %t7150, %fromInt32Unsafe_2 ], [ %t7205, %L_1776 ]
  %.sink5205 = phi i64 [ 55, %fromInt32Unsafe_3 ], [ 56, %L_1781 ], [ 57, %fromInt32Unsafe_2 ], [ 58, %L_1776 ]
  %stackTop.2.ph = phi i8* [ %stackTop.0, %fromInt32Unsafe_3 ], [ %t6744, %L_1781 ], [ %stackTop.0, %fromInt32Unsafe_2 ], [ %t7177, %L_1776 ]
  %frontier.2.ph = phi i8* [ %frontier.0, %fromInt32Unsafe_3 ], [ %frontier.70, %L_1781 ], [ %frontier.0, %fromInt32Unsafe_2 ], [ %frontier.76, %L_1776 ]
  %t6718 = bitcast i8* %t6717.sink to i64*
  store i64 %.sink5205, i64* %t6718, align 4
  br label %L_81

L_81:                                             ; preds = %L_81.sink.split, %doSwitchNextBlock
  %stackTop.2 = phi i8* [ %stackTop.0, %doSwitchNextBlock ], [ %stackTop.2.ph, %L_81.sink.split ]
  %frontier.2 = phi i8* [ %frontier.0, %doSwitchNextBlock ], [ %frontier.2.ph, %L_81.sink.split ]
  %t393 = load i8*, i8** %t15210, align 8
  %t395.not = icmp ult i8* %t393, %stackTop.2
  br i1 %t395.not, label %L_82, label %L_84

L_121:                                            ; preds = %L_120
  %t401 = getelementptr inbounds i8, i8* %stackTop.4, i64 32
  %t402 = bitcast i8* %t401 to i64*
  store i64 142, i64* %t402, align 4
  %t404 = getelementptr inbounds i8, i8* %stackTop.4, i64 40
  store i8* %frontier.4, i8** %t15197, align 8
  store i8* %t404, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t414 = load i8*, i8** %t15197, align 8
  %t417 = load i8*, i8** %t15200, align 8
  %t399 = getelementptr inbounds i8, i8* %t417, i64 -40
  br label %L_123

L_138:                                            ; preds = %L_135
  %t449 = lshr i64 %t481, 1
  %t451 = trunc i64 %t449 to i32
  %t453 = mul i32 %TW32_0.536255759, 10
  %17 = add i32 %TW32_0.536255759, 214748364
  %t456 = icmp ult i32 %17, 429496729
  %t442 = tail call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %t453, i32 %t451)
  %t443 = extractvalue { i32, i1 } %t442, 1
  %switch2164 = xor i1 %t443, true
  %or.cond2573 = select i1 %t456, i1 %switch2164, i1 false
  %t439 = sub i32 %t453, %t451
  br i1 %or.cond2573, label %L_132, label %L_1266.sink.split

L_137:                                            ; preds = %L_135
  %t460 = zext i32 %TW32_0.536255759 to i64
  %t463 = shl nuw nsw i64 %t460, 1
  %t465 = or i64 %t463, 1
  %t468 = bitcast i8* %stackTop.3 to i64*
  store i64 %t465, i64* %t468, align 4
  br label %doSwitchNextBlock.backedge

L_135:                                            ; preds = %L_134
  %t488 = getelementptr inbounds i8, i8* %t531, i64 %t501
  %t491 = load i8, i8* %t488, align 1
  %t495 = zext i8 %t491 to i64
  %t477 = shl nuw nsw i64 %t495, 3
  %t478 = getelementptr inbounds i8, i8* %t535, i64 %t477
  %t480 = bitcast i8* %t478 to i64*
  %t481 = load i64, i64* %t480, align 4
  %18 = and i64 %t481, 1
  %trunc1978.not = icmp eq i64 %18, 0
  br i1 %trunc1978.not, label %L_137, label %L_138

L_134:                                            ; preds = %L_132.preheader, %L_132
  %TW32_1.536265760 = phi i32 [ %TW32_1.53626, %L_132 ], [ %TW32_1.536265757, %L_132.preheader ]
  %TW32_0.536255759 = phi i32 [ %t439, %L_132 ], [ %t556, %L_132.preheader ]
  %t501 = sext i32 %TW32_1.536265760 to i64
  %t504.not = icmp ugt i64 %t539, %t501
  br i1 %t504.not, label %L_135, label %L_1266.sink.split

L_133:                                            ; preds = %L_132, %L_132.preheader
  %TW32_0.53625.lcssa = phi i32 [ %t556, %L_132.preheader ], [ %t439, %L_132 ]
  %t508 = zext i32 %TW32_0.53625.lcssa to i64
  %t511 = shl nuw nsw i64 %t508, 1
  %t513 = or i64 %t511, 1
  %t516 = bitcast i8* %stackTop.3 to i64*
  store i64 %t513, i64* %t516, align 4
  br label %doSwitchNextBlock.backedge

L_132:                                            ; preds = %L_138
  %TW32_1.53626 = add i32 %TW32_1.536265760, 1
  %t525.not = icmp slt i32 %TW32_1.53626, %t541
  br i1 %t525.not, label %L_134, label %L_133

L_131:                                            ; preds = %L_129
  %t558 = getelementptr inbounds i8, i8* %stackTop.3, i64 16
  %t559 = bitcast i8* %t558 to i8**
  %t560 = load i8*, i8** %t559, align 8
  %t529 = getelementptr inbounds i8, i8* %t560, i64 8
  %t530 = bitcast i8* %t529 to i8**
  %t531 = load i8*, i8** %t530, align 8
  %t534 = bitcast i8* %t560 to i8**
  %t535 = load i8*, i8** %t534, align 8
  %t537 = getelementptr inbounds i8, i8* %t531, i64 -16
  %t538 = bitcast i8* %t537 to i64*
  %t539 = load i64, i64* %t538, align 4
  %t541 = trunc i64 %t539 to i32
  %t543 = sext i32 %t541 to i64
  %t546.not = icmp eq i64 %t539, %t543
  br i1 %t546.not, label %L_132.preheader, label %L_1266.sink.split

L_132.preheader:                                  ; preds = %L_131
  %t552 = lshr i64 %t572, 1
  %t554 = trunc i64 %t552 to i32
  %t556 = sub i32 0, %t554
  %TW32_1.536265757 = add i32 %t636, 1
  %t525.not5758 = icmp slt i32 %TW32_1.536265757, %t541
  br i1 %t525.not5758, label %L_134, label %L_133

L_129:                                            ; preds = %L_127
  %t582 = getelementptr inbounds i8, i8* %t664, i64 %t619
  %t585 = load i8, i8* %t582, align 1
  %t592 = zext i8 %t585 to i64
  %t564 = getelementptr inbounds i8, i8* %stackTop.3, i64 8
  %t565 = bitcast i8* %t564 to i8**
  %t566 = load i8*, i8** %t565, align 8
  %t568 = shl nuw nsw i64 %t592, 3
  %t569 = getelementptr inbounds i8, i8* %t566, i64 %t568
  %t571 = bitcast i8* %t569 to i64*
  %t572 = load i64, i64* %t571, align 4
  %19 = and i64 %t572, 1
  %trunc1974.not = icmp eq i64 %19, 0
  br i1 %trunc1974.not, label %L_126, label %L_131

L_127:                                            ; preds = %L_125
  %t619 = sext i32 %t636 to i64
  %t622.not = icmp ugt i64 %t667, %t619
  br i1 %t622.not, label %L_129, label %L_1266.sink.split

L_126:                                            ; preds = %L_125, %L_129
  %t627 = bitcast i8* %stackTop.3 to i64*
  store i64 0, i64* %t627, align 4
  br label %doSwitchNextBlock.backedge

L_125:                                            ; preds = %L_123
  %t635 = bitcast i8* %stackTop.3 to i32*
  %t636 = load i32, i32* %t635, align 4
  %t638.not = icmp slt i32 %t636, %t669
  br i1 %t638.not, label %L_127, label %L_126

L_123:                                            ; preds = %L_120, %L_121
  %stackTop.3 = phi i8* [ %t399, %L_121 ], [ %stackTop.4, %L_120 ]
  %frontier.3 = phi i8* [ %t414, %L_121 ], [ %frontier.4, %L_120 ]
  %t662 = getelementptr inbounds i8, i8* %stackTop.3, i64 24
  %t663 = bitcast i8* %t662 to i8**
  %t664 = load i8*, i8** %t663, align 8
  %t665 = getelementptr inbounds i8, i8* %t664, i64 -16
  %t666 = bitcast i8* %t665 to i64*
  %t667 = load i64, i64* %t666, align 4
  %t669 = trunc i64 %t667 to i32
  %t671 = sext i32 %t669 to i64
  %t674.not = icmp eq i64 %t667, %t671
  br i1 %t674.not, label %L_125, label %L_1266.sink.split

L_120.sink.split:                                 ; preds = %L_1146, %negate_0, %L_1492
  %t10824.sink = phi i32 [ %t10824, %L_1492 ], [ %TW32_0.30, %negate_0 ], [ %t10824, %L_1146 ]
  %.sink5206 = phi i64 [ 54, %L_1492 ], [ 66, %negate_0 ], [ 53, %L_1146 ]
  %t6417.sink = getelementptr inbounds i8, i8* %stackTop.115, i64 176
  %t6418 = bitcast i8* %t6417.sink to i32*
  store i32 %t10824.sink, i32* %t6418, align 4
  %t6421 = getelementptr inbounds i8, i8* %stackTop.115, i64 184
  %t6422 = bitcast i8* %t6421 to i8**
  %t6426 = load i8*, i8** %t10879, align 8
  store i8* %t6426, i8** %t6422, align 8
  %t6428 = getelementptr inbounds i8, i8* %stackTop.115, i64 192
  %t6429 = bitcast i8* %t6428 to i8**
  store i8* %t10866, i8** %t6429, align 8
  %t6432 = getelementptr inbounds i8, i8* %stackTop.115, i64 200
  %t6433 = bitcast i8* %t6432 to i8**
  store i8* %t10813, i8** %t6433, align 8
  %t6439 = getelementptr inbounds i8, i8* %stackTop.115, i64 168
  %t6440 = bitcast i8* %t6439 to i64*
  store i64 %.sink5206, i64* %t6440, align 4
  br label %L_120

L_120:                                            ; preds = %L_120.sink.split, %doSwitchNextBlock
  %stackTop.4 = phi i8* [ %stackTop.0, %doSwitchNextBlock ], [ %t6417.sink, %L_120.sink.split ]
  %frontier.4 = phi i8* [ %frontier.0, %doSwitchNextBlock ], [ %t10873, %L_120.sink.split ]
  %t679 = load i8*, i8** %t15210, align 8
  %t681.not = icmp ult i8* %t679, %stackTop.4
  br i1 %t681.not, label %L_121, label %L_123

L_143:                                            ; preds = %L_142
  %t687 = getelementptr inbounds i8, i8* %stackTop.9, i64 16
  %t688 = bitcast i8* %t687 to i64*
  store i64 149, i64* %t688, align 4
  %t690 = getelementptr inbounds i8, i8* %stackTop.9, i64 24
  store i8* %frontier.9, i8** %t15197, align 8
  store i8* %t690, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t700 = load i8*, i8** %t15197, align 8
  %t703 = load i8*, i8** %t15200, align 8
  %t685 = getelementptr inbounds i8, i8* %t703, i64 -24
  br label %L_145

L_212:                                            ; preds = %L_194
  %t7054650 = urem i32 64, %TW32_0.8
  br label %L_198

L_205:                                            ; preds = %L_208
  %t734 = getelementptr inbounds i8, i8* %stackTop.8, i64 16
  %t735 = bitcast i8* %t734 to i64*
  store i64 153, i64* %t735, align 4
  %t737 = getelementptr inbounds i8, i8* %stackTop.8, i64 24
  store i8* %frontier.8, i8** %t15197, align 8
  store i8* %t737, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 %t794, i32 0)
  %t747 = load i8*, i8** %t15197, align 8
  %t750 = load i8*, i8** %t15200, align 8
  %t728 = getelementptr inbounds i8, i8* %t750, i64 -24
  %t762.phi.trans.insert = bitcast i8* %t728 to i8**
  %t763.pre = load i8*, i8** %t762.phi.trans.insert, align 8
  br label %L_207

L_207:                                            ; preds = %L_208, %L_205
  %t762.pre-phi = phi i8** [ %t1349, %L_208 ], [ %t762.phi.trans.insert, %L_205 ]
  %t763 = phi i8* [ %t1350, %L_208 ], [ %t763.pre, %L_205 ]
  %stackTop.5 = phi i8* [ %stackTop.8, %L_208 ], [ %t728, %L_205 ]
  %frontier.5 = phi i8* [ %frontier.8, %L_208 ], [ %t747, %L_205 ]
  %t765 = getelementptr inbounds i8, i8* %stackTop.5, i64 8
  %t766 = bitcast i8* %t765 to i64*
  %t767 = load i64, i64* %t766, align 4
  store i8* %frontier.5, i8** %t15197, align 8
  %t771 = tail call i8* @IntInf_toString(i8* nonnull %gcState, i8* %t763, i32 10, i64 %t767)
  %t774 = load i8*, i8** %t15197, align 8
  store i8* %t771, i8** %t762.pre-phi, align 8
  br label %doSwitchNextBlock.backedge

L_208:                                            ; preds = %L_198
  %t824.not = icmp ne i32 %TW32_0.7, 0
  %spec.select2178 = zext i1 %t824.not to i32
  %t808 = add nsw i64 %t806, -1
  %t816 = add nsw i32 %t858, %spec.select2178
  %t799 = zext i32 %t816 to i64
  %t792 = mul i64 %t808, %t799
  %t794 = add i64 %t792, 32
  %t796 = getelementptr inbounds i8, i8* %stackTop.8, i64 8
  %t797 = bitcast i8* %t796 to i64*
  store i64 %t794, i64* %t797, align 4
  %t777 = load i8*, i8** %t8218, align 8
  %t779 = ptrtoint i8* %t777 to i64
  %t780 = ptrtoint i8* %frontier.8 to i64
  %t781 = sub i64 %t779, %t780
  %t787.not = icmp ult i64 %t781, %t794
  br i1 %t787.not, label %L_205, label %L_207

L_198:                                            ; preds = %L_195, %L_212
  %TW32_0.7 = phi i32 [ %t7054650, %L_212 ], [ %t833, %L_195 ]
  %t804 = getelementptr inbounds i8, i8* %t1350, i64 -16
  %t805 = bitcast i8* %t804 to i64*
  %t806 = load i64, i64* %t805, align 4
  %t811.not = icmp eq i64 %t806, -9223372036854775808
  br i1 %t811.not, label %L_1266.sink.split, label %L_208

L_195:                                            ; preds = %L_194
  %TW32_0.8.nonneg = sub i32 0, %TW32_0.8
  %t8284649 = urem i32 63, %TW32_0.8.nonneg
  %t830 = add nsw i32 %TW32_0.8, 1
  %t833 = add nsw i32 %t830, %t8284649
  br label %L_198

L_194:                                            ; preds = %L_193
  %t858 = sdiv i32 64, %TW32_0.8
  %t860 = icmp slt i32 %TW32_0.8, 1
  br i1 %t860, label %L_195, label %L_212

L_193:                                            ; preds = %L_189
  %t864.not = icmp eq i32 %TW32_0.8, 0
  br i1 %t864.not, label %L_1266.sink.split, label %L_194

L_189:                                            ; preds = %L_145, %L_189
  %TW32_2.13623 = phi i32 [ %TW32_2.0, %L_189 ], [ 10, %L_145 ]
  %TW32_1.63622 = phi i32 [ %t869, %L_189 ], [ 16, %L_145 ]
  %TW32_0.93621 = phi i32 [ %TW32_0.8, %L_189 ], [ 0, %L_145 ]
  %TW32_2.1.highbits = lshr i32 %TW32_2.13623, %TW32_1.63622
  %t885.not = icmp eq i32 %TW32_2.1.highbits, 0
  %t877 = select i1 %t885.not, i32 0, i32 %TW32_1.63622
  %TW32_0.8 = add i32 %t877, %TW32_0.93621
  %TW32_2.0 = select i1 %t885.not, i32 %TW32_2.13623, i32 %TW32_2.1.highbits
  %t869 = lshr i32 %TW32_1.63622, 1
  %t889.not = icmp eq i32 %TW32_2.0, 1
  br i1 %t889.not, label %L_193, label %L_189

L_183:                                            ; preds = %L_182
  %t915 = add i32 %t939, -1
  store i32 %t915, i32* %t13969, align 4
  %t920 = getelementptr inbounds i8, i8* %stackTop.8, i64 40
  %t921 = bitcast i8* %t920 to i64*
  store i64 170, i64* %t921, align 4
  %t923 = getelementptr inbounds i8, i8* %stackTop.8, i64 48
  store i8* %frontier.8, i8** %t15197, align 8
  store i8* %t923, i8** %t15200, align 8
  %t930 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 65, i64 21)
  %t933 = load i8*, i8** %t15197, align 8
  %t936 = load i8*, i8** %t15200, align 8
  call void @llvm.memset.p0i8.i64(i8* noundef nonnull align 1 dereferenceable(65) %t930, i8 0, i64 65, i1 false)
  %t910 = getelementptr inbounds i8, i8* %t936, i64 -48
  %t903 = bitcast i8* %t910 to i8**
  store i8* %t930, i8** %t903, align 8
  br label %L_151

L_182:                                            ; preds = %L_146
  %t939 = load i32, i32* %t13969, align 4
  %t941.not = icmp eq i32 %t939, 0
  br i1 %t941.not, label %L_1266.sink.split, label %L_183

L_179:                                            ; preds = %L_178
  %t971 = load i8*, i8** %t1164, align 8
  %t974 = getelementptr inbounds i8, i8* %t971, i64 %t978
  store i8 126, i8* %t974, align 1
  br label %L_167

L_178:                                            ; preds = %L_177
  %t985 = add nsw i32 %TW32_1.736105748, -1
  %t978 = sext i32 %t985 to i64
  %t981.not = icmp ugt i64 %t11685749, %t978
  br i1 %t981.not, label %L_179, label %L_158

L_177:                                            ; preds = %L_165
  %t988.not = icmp eq i32 %TW32_1.736105748, -2147483648
  br i1 %t988.not, label %L_153, label %L_178

L_176:                                            ; preds = %L_176.lr.ph, %L_176
  %TW64_2.03620 = phi i64 [ 0, %L_176.lr.ph ], [ %t1014, %L_176 ]
  %t995 = add i64 %TW64_2.03620, %t1029
  %t999 = load i8*, i8** %t998, align 8
  %t1002 = getelementptr inbounds i8, i8* %t999, i64 %t995
  %t1005 = load i8, i8* %t1002, align 1
  %t1009 = getelementptr inbounds i8, i8* %t1091, i64 %TW64_2.03620
  store i8 %t1005, i8* %t1009, align 1
  %t1014 = add i64 %TW64_2.03620, 1
  %t1018.not = icmp slt i64 %t1025, %t1014
  br i1 %t1018.not, label %L_172, label %L_176

L_175:                                            ; preds = %L_join_0
  %t1025 = add i64 %t1069.pre, -1
  %t1027 = getelementptr inbounds i8, i8* %t1097, i64 -40
  %t1028 = bitcast i8* %t1027 to i64*
  %t1029 = load i64, i64* %t1028, align 4
  %t1018.not3619 = icmp slt i64 %t1025, 0
  br i1 %t1018.not3619, label %L_172, label %L_176.lr.ph

L_176.lr.ph:                                      ; preds = %L_175
  %t998 = bitcast i8* %t1074 to i8**
  br label %L_176

L_174:                                            ; preds = %L_172, %L_173
  %t1032 = bitcast i8* %stackTop.64506 to i8**
  store i8* %TP_0.04503, i8** %t1032, align 8
  br label %doSwitchNextBlock.backedge

L_173:                                            ; preds = %L_172
  %t1040 = getelementptr inbounds i8, i8* %stackTop.64506, i64 24
  %20 = bitcast i8* %t1040 to i32**
  %t10421914 = load i32*, i32** %20, align 8
  store i32 0, i32* %t10421914, align 4
  br label %L_174

L_172:                                            ; preds = %L_176, %L_168, %L_175, %L_170
  %frontier.64507 = phi i8* [ %t1094, %L_175 ], [ %t1094, %L_170 ], [ %frontier.7, %L_168 ], [ %t1094, %L_176 ]
  %stackTop.64506 = phi i8* [ %t1074, %L_175 ], [ %t1074, %L_170 ], [ %stackTop.7, %L_168 ], [ %t1074, %L_176 ]
  %TP_0.04503 = phi i8* [ %t1091, %L_175 ], [ %t1091, %L_170 ], [ getelementptr (i8, i8* @staticHeapM, i64 64), %L_168 ], [ %t1091, %L_176 ]
  %t1046 = getelementptr inbounds i8, i8* %TP_0.04503, i64 -8
  %t1047 = bitcast i8* %t1046 to i64*
  store i64 11, i64* %t1047, align 4
  %t1049 = getelementptr inbounds i8, i8* %stackTop.64506, i64 32
  %t1050 = bitcast i8* %t1049 to i32*
  %t1051 = load i32, i32* %t1050, align 4
  %switch2189 = icmp eq i32 %t1051, 0
  br i1 %switch2189, label %L_173, label %L_174

L_170:                                            ; preds = %L_join_0
  %t1055 = bitcast i8* %t1074 to i8**
  %t1056 = load i8*, i8** %t1055, align 8
  %t1058 = getelementptr inbounds i8, i8* %t1097, i64 -40
  %t1059 = bitcast i8* %t1058 to i64*
  %t1060 = load i64, i64* %t1059, align 4
  tail call void @GC_sequenceCopy(i8* nonnull %gcState, i8* %t1091, i64 0, i8* %t1056, i64 %t1060, i64 %t1069.pre)
  br label %L_172

L_join_0:                                         ; preds = %L_168
  %t1081 = getelementptr inbounds i8, i8* %stackTop.7, i64 40
  %t1082 = bitcast i8* %t1081 to i64*
  store i64 169, i64* %t1082, align 4
  %t1084 = getelementptr inbounds i8, i8* %stackTop.7, i64 48
  store i8* %frontier.7, i8** %t15197, align 8
  store i8* %t1084, i8** %t15200, align 8
  %t1091 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t1103, i64 21)
  %t1094 = load i8*, i8** %t15197, align 8
  %t1097 = load i8*, i8** %t15200, align 8
  %t1074 = getelementptr inbounds i8, i8* %t1097, i64 -48
  %t1067.phi.trans.insert = getelementptr inbounds i8, i8* %t1097, i64 -32
  %t1068.phi.trans.insert = bitcast i8* %t1067.phi.trans.insert to i64*
  %t1069.pre = load i64, i64* %t1068.phi.trans.insert, align 4
  %t1070 = icmp sgt i64 %t1069.pre, 4
  br i1 %t1070, label %L_170, label %L_175

L_168:                                            ; preds = %L_167
  %t1103 = sub i64 %t11685749, %t1115.pre-phi
  store i64 %t1103, i64* %t1270, align 4
  %t1111.not = icmp eq i64 %t1103, 0
  br i1 %t1111.not, label %L_172, label %L_join_0

L_167:                                            ; preds = %L_165, %L_179
  %t1115.pre-phi = phi i64 [ %t11705750, %L_165 ], [ %t978, %L_179 ]
  %t1117 = getelementptr inbounds i8, i8* %stackTop.7, i64 8
  %t1118 = bitcast i8* %t1117 to i64*
  store i64 %t1115.pre-phi, i64* %t1118, align 4
  %t1124.not = icmp ult i64 %t11685749, %t1115.pre-phi
  br i1 %t1124.not, label %L_158, label %L_168

L_165:                                            ; preds = %L_162
  br i1 %trunc1902, label %L_167, label %L_177

L_164:                                            ; preds = %L_163
  %t1251 = srem i64 %t1149, 10
  %t1253 = sub nsw i64 0, %t1251
  %t1200 = icmp ugt i64 %t1253, 15
  br i1 %t1200, label %L_158, label %L_164.L_161_crit_edge

L_164.L_161_crit_edge:                            ; preds = %L_164
  %t1132 = add nsw i32 %TW32_1.736105748, -1
  %t1165.pre = load i8*, i8** %t1164, align 8
  %t1166 = getelementptr inbounds i8, i8* %t1165.pre, i64 -16
  %t1167 = bitcast i8* %t1166 to i64*
  %t1168 = load i64, i64* %t1167, align 4
  %t1170 = sext i32 %t1132 to i64
  %t1173.not = icmp ugt i64 %t1168, %t1170
  br i1 %t1173.not, label %L_162, label %L_158

L_163:                                            ; preds = %L_162
  %t1135.not = icmp eq i32 %TW32_1.736105748, -2147483648
  br i1 %t1135.not, label %L_153, label %L_164

L_162:                                            ; preds = %L_161.lr.ph, %L_164.L_161_crit_edge
  %t11705750 = phi i64 [ %t1170, %L_164.L_161_crit_edge ], [ 64, %L_161.lr.ph ]
  %t11685749 = phi i64 [ %t1168, %L_164.L_161_crit_edge ], [ %t11685743, %L_161.lr.ph ]
  %TW32_1.736105748 = phi i32 [ %t1132, %L_164.L_161_crit_edge ], [ 64, %L_161.lr.ph ]
  %TW64_0.136115747 = phi i64 [ %t1149, %L_164.L_161_crit_edge ], [ %spec.select2201, %L_161.lr.ph ]
  %t125336125746 = phi i64 [ %t1253, %L_164.L_161_crit_edge ], [ %t12533608, %L_161.lr.ph ]
  %t11655745 = phi i8* [ %t1165.pre, %L_164.L_161_crit_edge ], [ %t11654487, %L_161.lr.ph ]
  %t1158 = getelementptr inbounds i8, i8* getelementptr (i8, i8* @staticHeapI, i64 4696), i64 %t125336125746
  %t1161 = load i8, i8* %t1158, align 1
  %t1144 = getelementptr inbounds i8, i8* %t11655745, i64 %t11705750
  store i8 %t1161, i8* %t1144, align 1
  %t1149 = sdiv i64 %TW64_0.136115747, 10
  %21 = add i64 %TW64_0.136115747, -10
  %22 = icmp ult i64 %21, -19
  br i1 %22, label %L_163, label %L_165

L_158:                                            ; preds = %L_167, %L_178, %L_151, %L_161.lr.ph, %L_164, %L_164.L_161_crit_edge
  %t1196 = getelementptr inbounds i8, i8* %stackTop.7, i64 32
  %t1197 = bitcast i8* %t1196 to i32*
  %t1198 = load i32, i32* %t1197, align 4
  %switch2197 = icmp eq i32 %t1198, 0
  br i1 %switch2197, label %L_155, label %L_1266.sink.split

L_155:                                            ; preds = %L_153, %L_158
  %TP_0.1 = phi i8* [ inttoptr (i64 6 to i8*), %L_158 ], [ inttoptr (i64 1 to i8*), %L_153 ]
  %t1222 = getelementptr inbounds i8, i8* %stackTop.7, i64 24
  %23 = bitcast i8* %t1222 to i32**
  %t12241908 = load i32*, i32** %23, align 8
  store i32 0, i32* %t12241908, align 4
  br label %L_1266.sink.split

L_153:                                            ; preds = %L_177, %L_163
  %t1247 = getelementptr inbounds i8, i8* %stackTop.7, i64 32
  %t1248 = bitcast i8* %t1247 to i32*
  %t1249 = load i32, i32* %t1248, align 4
  %switch2198 = icmp eq i32 %t1249, 0
  br i1 %switch2198, label %L_155, label %L_1266.sink.split

L_151:                                            ; preds = %L_149, %L_183
  %t11654487 = phi i8* [ %t930, %L_183 ], [ %t1315, %L_149 ]
  %stackTop.7 = phi i8* [ %t910, %L_183 ], [ %stackTop.8, %L_149 ]
  %frontier.7 = phi i8* [ %t933, %L_183 ], [ %frontier.8, %L_149 ]
  %t1269 = getelementptr inbounds i8, i8* %stackTop.7, i64 16
  %t1270 = bitcast i8* %t1269 to i64*
  %t1271 = load i64, i64* %t1270, align 4
  %trunc1902 = icmp sgt i64 %t1271, -1
  %t1267 = sub i64 0, %t1271
  %spec.select2201 = select i1 %trunc1902, i64 %t1267, i64 %t1271
  %t12513607 = srem i64 %spec.select2201, 10
  %t12533608 = sub nsw i64 0, %t12513607
  %t12003609 = icmp ugt i64 %t12533608, 15
  br i1 %t12003609, label %L_158, label %L_161.lr.ph

L_161.lr.ph:                                      ; preds = %L_151
  %t1164 = bitcast i8* %stackTop.7 to i8**
  %t11665741 = getelementptr inbounds i8, i8* %t11654487, i64 -16
  %t11675742 = bitcast i8* %t11665741 to i64*
  %t11685743 = load i64, i64* %t11675742, align 4
  %t1173.not5744 = icmp ugt i64 %t11685743, 64
  br i1 %t1173.not5744, label %L_162, label %L_158

L_149:                                            ; preds = %L_148
  %t1278 = add i32 %t1294, -1
  store i32 %t1278, i32* %t13969, align 4
  store i8* %t1315, i8** %t1349, align 8
  br label %L_151

L_148:                                            ; preds = %L_146
  store i32 1, i32* %t13391899, align 4
  %t1294 = load i32, i32* %t13969, align 4
  %t1296.not = icmp eq i32 %t1294, 0
  br i1 %t1296.not, label %L_1266.sink.split, label %L_149

L_146:                                            ; preds = %L_145
  %t1305 = ashr i64 %t1351, 1
  %t1307 = getelementptr inbounds i8, i8* %stackTop.8, i64 16
  %t1308 = bitcast i8* %t1307 to i64*
  store i64 %t1305, i64* %t1308, align 4
  %t1310 = getelementptr inbounds i8, i8* %stackTop.8, i64 8
  %t1311 = bitcast i8* %t1310 to i8**
  %t1312 = load i8*, i8** %t1311, align 8
  %t1313 = getelementptr inbounds i8, i8* %t1312, i64 8
  %t1314 = bitcast i8* %t1313 to i8**
  %t1315 = load i8*, i8** %t1314, align 8
  %t1317 = getelementptr inbounds i8, i8* %stackTop.8, i64 24
  %t1318 = bitcast i8* %t1317 to i8**
  %t1324 = bitcast i8* %t1312 to i8**
  %t1325 = load i8*, i8** %t1324, align 8
  store i8* %t1325, i8** %t1318, align 8
  %t1328 = load i32, i32* %t13969, align 4
  %t1329 = add i32 %t1328, 1
  store i32 %t1329, i32* %t13969, align 4
  %t1334 = getelementptr inbounds i8, i8* %stackTop.8, i64 32
  %t1335 = bitcast i8* %t1334 to i32*
  %24 = bitcast i8* %t1317 to i32**
  %t13391899 = load i32*, i32** %24, align 8
  %t1342 = load i32, i32* %t13391899, align 4
  store i32 %t1342, i32* %t1335, align 4
  %switch2203 = icmp eq i32 %t1342, 0
  br i1 %switch2203, label %L_148, label %L_182

L_145:                                            ; preds = %L_142, %L_143
  %stackTop.8 = phi i8* [ %t685, %L_143 ], [ %stackTop.9, %L_142 ]
  %frontier.8 = phi i8* [ %t700, %L_143 ], [ %frontier.9, %L_142 ]
  %t1349 = bitcast i8* %stackTop.8 to i8**
  %t1350 = load i8*, i8** %t1349, align 8
  %t1351 = ptrtoint i8* %t1350 to i64
  %25 = and i64 %t1351, 1
  %.not1890.not = icmp eq i64 %25, 0
  br i1 %.not1890.not, label %L_189, label %L_146

L_142.sink.split:                                 ; preds = %L_217, %L_266, %L_263, %L_256, %L_247
  %TP_0.3.sink = phi i8* [ %t1433, %L_263 ], [ %t1492, %L_256 ], [ %t1606, %L_247 ], [ %t1899, %L_217 ], [ %t1875.pre, %L_266 ]
  %stackTop.12.sink5209 = phi i8* [ %stackTop.10, %L_263 ], [ %stackTop.16, %L_256 ], [ %stackTop.16, %L_247 ], [ %stackTop.16, %L_217 ], [ %stackTop.16, %L_266 ]
  %.sink5207 = phi i64 [ 43, %L_263 ], [ 43, %L_256 ], [ 43, %L_247 ], [ 44, %L_217 ], [ 44, %L_266 ]
  %frontier.9.ph = phi i8* [ %t1436, %L_263 ], [ %frontier.18, %L_256 ], [ %frontier.18, %L_247 ], [ %frontier.18, %L_217 ], [ %frontier.18, %L_266 ]
  %t1584.sink = getelementptr inbounds i8, i8* %stackTop.12.sink5209, i64 48
  %t1585 = bitcast i8* %t1584.sink to i8**
  store i8* %TP_0.3.sink, i8** %t1585, align 8
  %t1588 = getelementptr inbounds i8, i8* %stackTop.12.sink5209, i64 56
  %t1589 = bitcast i8* %t1588 to i8**
  %t1591 = getelementptr inbounds i8, i8* %stackTop.12.sink5209, i64 24
  %t1592 = bitcast i8* %t1591 to i8**
  %t1593 = load i8*, i8** %t1592, align 8
  store i8* %t1593, i8** %t1589, align 8
  %t1595 = getelementptr inbounds i8, i8* %stackTop.12.sink5209, i64 40
  %t1596 = bitcast i8* %t1595 to i64*
  store i64 %.sink5207, i64* %t1596, align 4
  br label %L_142

L_142:                                            ; preds = %L_142.sink.split, %doSwitchNextBlock
  %stackTop.9 = phi i8* [ %stackTop.0, %doSwitchNextBlock ], [ %t1584.sink, %L_142.sink.split ]
  %frontier.9 = phi i8* [ %frontier.0, %doSwitchNextBlock ], [ %frontier.9.ph, %L_142.sink.split ]
  %t1359 = load i8*, i8** %t15210, align 8
  %t1361.not = icmp ult i8* %t1359, %stackTop.9
  br i1 %t1361.not, label %L_143, label %L_145

L_214:                                            ; preds = %L_213
  %t1367 = getelementptr inbounds i8, i8* %stackTop.17, i64 40
  %t1368 = bitcast i8* %t1367 to i64*
  store i64 168, i64* %t1368, align 4
  %t1370 = getelementptr inbounds i8, i8* %stackTop.17, i64 48
  store i8* %frontier.19, i8** %t15197, align 8
  store i8* %t1370, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t1380 = load i8*, i8** %t15197, align 8
  %t1383 = load i8*, i8** %t15200, align 8
  %t1365 = getelementptr inbounds i8, i8* %t1383, i64 -48
  br label %L_216

L_266:                                            ; preds = %L_216
  %t1394 = tail call i32 @IntInf_compare(i8* nonnull %gcState, i8* %t1899, i8* nonnull inttoptr (i64 1 to i8*))
  %trunc1883 = icmp sgt i32 %t1394, -1
  %t1875.pre = load i8*, i8** %t1898, align 8
  br i1 %trunc1883, label %L_142.sink.split, label %L_257

L_261:                                            ; preds = %L_264
  %t1404 = getelementptr inbounds i8, i8* %stackTop.16, i64 40
  %t1405 = bitcast i8* %t1404 to i64*
  store i64 167, i64* %t1405, align 4
  %t1407 = getelementptr inbounds i8, i8* %stackTop.16, i64 48
  store i8* %frontier.18, i8** %t15197, align 8
  store i8* %t1407, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 %t1471, i32 0)
  %t1417 = load i8*, i8** %t15197, align 8
  %t1420 = load i8*, i8** %t15200, align 8
  %t1398 = getelementptr inbounds i8, i8* %t1420, i64 -48
  %t1424.phi.trans.insert = bitcast i8* %t1398 to i8**
  %t1425.pre = load i8*, i8** %t1424.phi.trans.insert, align 8
  br label %L_263

L_263:                                            ; preds = %L_264, %L_261
  %t1425 = phi i8* [ %t1425.pre, %L_261 ], [ %t1875.pre, %L_264 ]
  %stackTop.10 = phi i8* [ %t1398, %L_261 ], [ %stackTop.16, %L_264 ]
  %frontier.10 = phi i8* [ %t1417, %L_261 ], [ %frontier.18, %L_264 ]
  %t1427 = getelementptr inbounds i8, i8* %stackTop.10, i64 8
  %t1428 = bitcast i8* %t1427 to i64*
  %t1429 = load i64, i64* %t1428, align 4
  store i8* %frontier.10, i8** %t15197, align 8
  %t1433 = tail call i8* @IntInf_neg(i8* nonnull %gcState, i8* %t1425, i64 %t1429)
  %t1436 = load i8*, i8** %t15197, align 8
  br label %L_142.sink.split

L_264:                                            ; preds = %L_257
  %t1454 = getelementptr inbounds i8, i8* %stackTop.16, i64 16
  %t1455 = bitcast i8* %t1454 to i64*
  %t1456 = load i64, i64* %t1455, align 4
  %reass.add = add i64 %t1481, 1
  %reass.mul2648 = mul i64 %t1456, %reass.add
  %t1471 = add i64 %reass.mul2648, 31
  %t1473 = getelementptr inbounds i8, i8* %stackTop.16, i64 8
  %t1474 = bitcast i8* %t1473 to i64*
  store i64 %t1471, i64* %t1474, align 4
  %t1439 = load i8*, i8** %t8218, align 8
  %t1441 = ptrtoint i8* %t1439 to i64
  %t1442 = ptrtoint i8* %frontier.18 to i64
  %t1443 = sub i64 %t1441, %t1442
  %t1449.not = icmp ult i64 %t1443, %t1471
  br i1 %t1449.not, label %L_261, label %L_263

L_257:                                            ; preds = %L_266
  %t1479 = getelementptr inbounds i8, i8* %t1875.pre, i64 -16
  %t1480 = bitcast i8* %t1479 to i64*
  %t1481 = load i64, i64* %t1480, align 4
  %t1486.not = icmp eq i64 %t1481, -9223372036854775808
  br i1 %t1486.not, label %L_1266.sink.split, label %L_264

L_256:                                            ; preds = %L_246
  %t1490 = getelementptr inbounds i8, i8* %stackTop.16, i64 8
  %t1491 = bitcast i8* %t1490 to i8**
  %t1492 = load i8*, i8** %t1491, align 8
  br label %L_142.sink.split

L_255:                                            ; preds = %L_249
  store i8* getelementptr (i8, i8* @staticHeapI, i64 4736), i8** %t1561, align 8
  br label %L_220

L_join_2:                                         ; preds = %L_251, %L_nonZeroLen_2
  %t1511.pre-phi = phi i8** [ %t1561, %L_251 ], [ %.pre, %L_nonZeroLen_2 ]
  %TP_0.2 = phi i8* [ getelementptr (i8, i8* @staticHeapM, i64 64), %L_251 ], [ %t1537, %L_nonZeroLen_2 ]
  %stackTop.11 = phi i8* [ %t1558, %L_251 ], [ %t1523, %L_nonZeroLen_2 ]
  %frontier.11 = phi i8* [ %frontier.0, %L_251 ], [ %t1540, %L_nonZeroLen_2 ]
  tail call void @GC_sequenceCopy(i8* %gcState, i8* %TP_0.2, i64 0, i8* getelementptr (i8, i8* @staticHeapI, i64 4736), i64 0, i64 1)
  %t1512 = load i8*, i8** %t1511.pre-phi, align 8
  %t1514 = getelementptr inbounds i8, i8* %stackTop.11, i64 8
  %t1515 = bitcast i8* %t1514 to i64*
  %t1516 = load i64, i64* %t1515, align 4
  tail call void @GC_sequenceCopy(i8* %gcState, i8* %TP_0.2, i64 1, i8* %t1512, i64 0, i64 %t1516)
  %t1501 = getelementptr inbounds i8, i8* %TP_0.2, i64 -8
  %t1502 = bitcast i8* %t1501 to i64*
  store i64 11, i64* %t1502, align 4
  store i8* %TP_0.2, i8** %t1511.pre-phi, align 8
  br label %L_220

L_nonZeroLen_2:                                   ; preds = %L_251
  %t1527 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t1528 = bitcast i8* %t1527 to i64*
  store i64 166, i64* %t1528, align 4
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %stackTop.0, i8** %t15200, align 8
  %t1537 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t1552, i64 21)
  %t1540 = load i8*, i8** %t15197, align 8
  %t1543 = load i8*, i8** %t15200, align 8
  %t1523 = getelementptr inbounds i8, i8* %t1543, i64 -48
  %.pre = bitcast i8* %t1523 to i8**
  br label %L_join_2

L_251:                                            ; preds = %L_250
  %t1545.not = icmp eq i64 %t1552, 0
  br i1 %t1545.not, label %L_join_2, label %L_nonZeroLen_2

L_250:                                            ; preds = %L_249
  %t1552 = add i64 %t1575, 1
  %t1554 = icmp ult i64 %t1552, 2147483648
  br i1 %t1554, label %L_251, label %L_1266.sink.split

L_249:                                            ; preds = %doSwitchNextBlock
  %t1558 = getelementptr inbounds i8, i8* %stackTop.0, i64 -48
  %t1561 = bitcast i8* %t1558 to i8**
  %t1564 = bitcast i8* %stackTop.0 to i8**
  %t1565 = load i8*, i8** %t1564, align 8
  store i8* %t1565, i8** %t1561, align 8
  %t1567 = getelementptr inbounds i8, i8* %stackTop.0, i64 -40
  %t1568 = bitcast i8* %t1567 to i64*
  %t1573 = getelementptr inbounds i8, i8* %t1565, i64 -16
  %t1574 = bitcast i8* %t1573 to i64*
  %t1575 = load i64, i64* %t1574, align 4
  store i64 %t1575, i64* %t1568, align 4
  %t1580.not = icmp eq i64 %t1575, 0
  br i1 %t1580.not, label %L_255, label %L_250

L_247:                                            ; preds = %L_246
  %t1604 = sub i64 2, %t1900
  %t1606 = inttoptr i64 %t1604 to i8*
  br label %L_142.sink.split

L_246:                                            ; preds = %L_217
  %t1612.not = icmp eq i8* %t1899, inttoptr (i64 -9223372036854775807 to i8*)
  br i1 %t1612.not, label %L_256, label %L_247

L_236.fold.split:                                 ; preds = %L_229
  br label %L_236

L_236.fold.split2637:                             ; preds = %L_229
  br label %L_236

L_236.fold.split2638:                             ; preds = %L_229
  br label %L_236

L_236:                                            ; preds = %L_229, %L_236.fold.split2638, %L_236.fold.split2637, %L_236.fold.split
  %TW32_0.11 = phi i32 [ 1, %L_229 ], [ 0, %L_236.fold.split ], [ 2, %L_236.fold.split2637 ], [ 3, %L_236.fold.split2638 ]
  %t1676 = tail call double @Real64_strtor(i8* nonnull %TP_0.4, i32 %TW32_0.11)
  %t1668 = bitcast i8* %stackTop.13 to double*
  store double %t1676, double* %t1668, align 8
  br label %doSwitchNextBlock.backedge

L_229:                                            ; preds = %L_227
  %t1714 = tail call i32 @IEEEReal_getRoundingMode()
  switch i32 %t1714, label %L_1266.sink.split [
    i32 0, label %L_236
    i32 1024, label %L_236.fold.split2638
    i32 2048, label %L_236.fold.split2637
    i32 3072, label %L_236.fold.split
  ]

L_227:                                            ; preds = %fromString_0
  %t1751 = add nsw i64 %t1749, -1
  %t1738 = getelementptr inbounds i8, i8* %TP_0.4, i64 %t1751
  %t1741 = load i8, i8* %t1738, align 1
  %t1743.not = icmp eq i8 %t1741, 0
  br i1 %t1743.not, label %L_229, label %L_1266.sink.split

fromString_0:                                     ; preds = %L_221, %L_220, %L_join_1
  %TP_0.4 = phi i8* [ %TP_0.5, %L_join_1 ], [ %t1848, %L_220 ], [ %t1838, %L_221 ]
  %stackTop.13 = phi i8* [ %stackTop.14, %L_join_1 ], [ %stackTop.15, %L_220 ], [ %stackTop.15, %L_221 ]
  %frontier.15 = phi i8* [ %frontier.16, %L_join_1 ], [ %frontier.17, %L_220 ], [ %frontier.17, %L_221 ]
  %t1747 = getelementptr inbounds i8, i8* %TP_0.4, i64 -16
  %t1748 = bitcast i8* %t1747 to i64*
  %t1749 = load i64, i64* %t1748, align 4
  %t1754.not = icmp eq i64 %t1749, -9223372036854775808
  br i1 %t1754.not, label %L_1266.sink.split, label %L_227

L_join_1:                                         ; preds = %L_223, %L_nonZeroLen_1
  %t1783.pre-phi = phi i64* [ %t1834, %L_223 ], [ %t1783.phi.trans.insert, %L_nonZeroLen_1 ]
  %t1784 = phi i64 [ %t1841, %L_223 ], [ %t1784.pre, %L_nonZeroLen_1 ]
  %t1780 = phi i8* [ %t1838, %L_223 ], [ %t1780.pre, %L_nonZeroLen_1 ]
  %TP_0.5 = phi i8* [ getelementptr (i8, i8* @staticHeapM, i64 64), %L_223 ], [ %t1801, %L_nonZeroLen_1 ]
  %stackTop.14 = phi i8* [ %stackTop.15, %L_223 ], [ %t1787, %L_nonZeroLen_1 ]
  %frontier.16 = phi i8* [ %frontier.17, %L_223 ], [ %t1804, %L_nonZeroLen_1 ]
  tail call void @GC_sequenceCopy(i8* %gcState, i8* %TP_0.5, i64 0, i8* %t1780, i64 0, i64 %t1784)
  %t1766 = load i64, i64* %t1783.pre-phi, align 4
  %t1768 = getelementptr inbounds i8, i8* %stackTop.14, i64 32
  %t1769 = bitcast i8* %t1768 to i8**
  %t1770 = load i8*, i8** %t1769, align 8
  %t1772 = getelementptr inbounds i8, i8* %stackTop.14, i64 16
  %t1773 = bitcast i8* %t1772 to i64*
  %t1774 = load i64, i64* %t1773, align 4
  tail call void @GC_sequenceCopy(i8* %gcState, i8* %TP_0.5, i64 %t1766, i8* %t1770, i64 0, i64 %t1774)
  %t1758 = getelementptr inbounds i8, i8* %TP_0.5, i64 -8
  %t1759 = bitcast i8* %t1758 to i64*
  store i64 11, i64* %t1759, align 4
  br label %fromString_0

L_nonZeroLen_1:                                   ; preds = %L_223
  %t1791 = getelementptr inbounds i8, i8* %stackTop.15, i64 40
  %t1792 = bitcast i8* %t1791 to i64*
  store i64 166, i64* %t1792, align 4
  %t1794 = getelementptr inbounds i8, i8* %stackTop.15, i64 48
  store i8* %frontier.17, i8** %t15197, align 8
  store i8* %t1794, i8** %t15200, align 8
  %t1801 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t1820, i64 21)
  %t1804 = load i8*, i8** %t15197, align 8
  %t1807 = load i8*, i8** %t15200, align 8
  %t1787 = getelementptr inbounds i8, i8* %t1807, i64 -48
  %t1779.phi.trans.insert = bitcast i8* %t1787 to i8**
  %t1780.pre = load i8*, i8** %t1779.phi.trans.insert, align 8
  %t1782.phi.trans.insert = getelementptr inbounds i8, i8* %t1807, i64 -40
  %t1783.phi.trans.insert = bitcast i8* %t1782.phi.trans.insert to i64*
  %t1784.pre = load i64, i64* %t1783.phi.trans.insert, align 4
  br label %L_join_1

L_223:                                            ; preds = %L_222
  %t1809.not = icmp eq i64 %t1820, 0
  br i1 %t1809.not, label %L_join_1, label %L_nonZeroLen_1

L_222:                                            ; preds = %L_221
  %t1820 = add i64 %t1851, %t1841
  %t1822 = icmp ult i64 %t1820, 2147483648
  br i1 %t1822, label %L_223, label %L_1266.sink.split

L_221:                                            ; preds = %L_220
  %t1829.not = icmp eq i64 %t1851, 0
  br i1 %t1829.not, label %fromString_0, label %L_222

L_220:                                            ; preds = %L_219, %L_join_2, %L_255
  %t1838 = phi i8* [ %t1864, %L_219 ], [ getelementptr (i8, i8* @staticHeapI, i64 4736), %L_255 ], [ %TP_0.2, %L_join_2 ]
  %stackTop.15 = phi i8* [ %t1860, %L_219 ], [ %t1558, %L_255 ], [ %stackTop.11, %L_join_2 ]
  %frontier.17 = phi i8* [ %frontier.0, %L_219 ], [ %frontier.0, %L_255 ], [ %frontier.11, %L_join_2 ]
  %t1833 = getelementptr inbounds i8, i8* %stackTop.15, i64 8
  %t1834 = bitcast i8* %t1833 to i64*
  %t1839 = getelementptr inbounds i8, i8* %t1838, i64 -16
  %t1840 = bitcast i8* %t1839 to i64*
  %t1841 = load i64, i64* %t1840, align 4
  store i64 %t1841, i64* %t1834, align 4
  %t1843 = getelementptr inbounds i8, i8* %stackTop.15, i64 16
  %t1844 = bitcast i8* %t1843 to i64*
  %t1846 = getelementptr inbounds i8, i8* %stackTop.15, i64 32
  %t1847 = bitcast i8* %t1846 to i8**
  %t1848 = load i8*, i8** %t1847, align 8
  %t1849 = getelementptr inbounds i8, i8* %t1848, i64 -16
  %t1850 = bitcast i8* %t1849 to i64*
  %t1851 = load i64, i64* %t1850, align 4
  store i64 %t1851, i64* %t1844, align 4
  %t1856.not = icmp eq i64 %t1841, 0
  br i1 %t1856.not, label %fromString_0, label %L_221

L_219:                                            ; preds = %doSwitchNextBlock
  %t1860 = getelementptr inbounds i8, i8* %stackTop.0, i64 -48
  %t1863 = bitcast i8* %stackTop.0 to i8**
  %t1864 = load i8*, i8** %t1863, align 8
  %t1867 = bitcast i8* %t1860 to i8**
  store i8* %t1864, i8** %t1867, align 8
  br label %L_220

L_217:                                            ; preds = %L_216
  %t1893.not = icmp slt i8* %t1899, inttoptr (i64 1 to i8*)
  br i1 %t1893.not, label %L_246, label %L_142.sink.split

L_216:                                            ; preds = %L_213, %L_214
  %stackTop.16 = phi i8* [ %t1365, %L_214 ], [ %stackTop.17, %L_213 ]
  %frontier.18 = phi i8* [ %t1380, %L_214 ], [ %frontier.19, %L_213 ]
  %t1898 = bitcast i8* %stackTop.16 to i8**
  %t1899 = load i8*, i8** %t1898, align 8
  %t1900 = ptrtoint i8* %t1899 to i64
  %26 = and i64 %t1900, 1
  %trunc1882.not = icmp eq i64 %26, 0
  br i1 %trunc1882.not, label %L_266, label %L_217

L_213.sink.split:                                 ; preds = %L_1734, %L_1218
  %t9794.sink = phi i8* [ %t9794, %L_1218 ], [ %t11424, %L_1734 ]
  %.sink5210 = phi i64 [ 63, %L_1218 ], [ 67, %L_1734 ]
  %stackTop.17.ph = phi i8* [ %t9764, %L_1218 ], [ %t11522, %L_1734 ]
  %frontier.19.ph = phi i8* [ %frontier.99, %L_1218 ], [ %t11519, %L_1734 ]
  %t9795 = bitcast i8* %t9794.sink to i64*
  store i64 %.sink5210, i64* %t9795, align 4
  br label %L_213

L_213:                                            ; preds = %L_213.sink.split, %doSwitchNextBlock
  %stackTop.17 = phi i8* [ %stackTop.0, %doSwitchNextBlock ], [ %stackTop.17.ph, %L_213.sink.split ]
  %frontier.19 = phi i8* [ %frontier.0, %doSwitchNextBlock ], [ %frontier.19.ph, %L_213.sink.split ]
  %t1910 = load i8*, i8** %t15210, align 8
  %t1912.not = icmp ult i8* %t1910, %stackTop.17
  br i1 %t1912.not, label %L_214, label %L_216

L_598:                                            ; preds = %L_597
  %t1918 = getelementptr inbounds i8, i8* %stackTop.24, i64 16
  %t1919 = bitcast i8* %t1918 to i64*
  store i64 151, i64* %t1919, align 4
  %t1921 = getelementptr inbounds i8, i8* %stackTop.24, i64 24
  store i8* %frontier.26, i8** %t15197, align 8
  store i8* %t1921, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t1931 = load i8*, i8** %t15197, align 8
  %t1934 = load i8*, i8** %t15200, align 8
  %t1916 = getelementptr inbounds i8, i8* %t1934, i64 -24
  br label %L_600

L_637:                                            ; preds = %L_600
  %t1940 = shl nsw i64 %t2327, 1
  %t1942 = or i64 %t1940, 1
  %t1944 = inttoptr i64 %t1942 to i8*
  br label %x_3

L_1812:                                           ; preds = %loop_21
  %t1972 = bitcast i8* %stackTop.22 to i64*
  store i64 %TW64_1.2, i64* %t1972, align 4
  %t1975 = getelementptr inbounds i8, i8* %stackTop.22, i64 16
  %t1976 = bitcast i8* %t1975 to i64*
  store i64 %TW64_0.4, i64* %t1976, align 4
  %t1979 = getelementptr inbounds i8, i8* %stackTop.22, i64 24
  %t1980 = bitcast i8* %t1979 to i8**
  store i8* %TP_0.10, i8** %t1980, align 8
  %t1983 = getelementptr inbounds i8, i8* %stackTop.22, i64 32
  %t1984 = bitcast i8* %t1983 to i64*
  store i64 150, i64* %t1984, align 4
  %t1986 = getelementptr inbounds i8, i8* %stackTop.22, i64 40
  store i8* %frontier.24, i8** %t15197, align 8
  store i8* %t1986, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t1996 = load i8*, i8** %t15197, align 8
  %t1999 = load i8*, i8** %t15200, align 8
  %t1954 = getelementptr inbounds i8, i8* %t1999, i64 -40
  %t1957 = bitcast i8* %t1954 to i64*
  %t1958 = load i64, i64* %t1957, align 4
  %t1960 = getelementptr inbounds i8, i8* %t1999, i64 -24
  %t1961 = bitcast i8* %t1960 to i64*
  %t1962 = load i64, i64* %t1961, align 4
  %t1964 = getelementptr inbounds i8, i8* %t1999, i64 -16
  %t1965 = bitcast i8* %t1964 to i8**
  %t1966 = load i8*, i8** %t1965, align 8
  br label %L_606

L_zeroLen_6:                                      ; preds = %L_1811
  %cond67 = icmp eq i8* %TP_0.9, inttoptr (i64 1 to i8*)
  br i1 %cond67, label %L_616, label %L_613.preheader

L_632:                                            ; preds = %L_630
  %t2025 = load i8*, i8** %t14423, align 8
  %t2028 = load i64, i64* %t15180, align 4
  %t2029 = getelementptr inbounds i8, i8* %t2025, i64 %t2028
  br label %doSwitchNextBlock.backedge

L_631:                                            ; preds = %L_630
  %t2040 = add nsw i64 %t2038, -1
  br label %L_622

L_630:                                            ; preds = %L_620
  %t2036 = getelementptr inbounds i8, i8* %TP_0.6, i64 -16
  %t2037 = bitcast i8* %t2036 to i64*
  %t2038 = load i64, i64* %t2037, align 4
  %t2043.not = icmp eq i64 %t2038, -9223372036854775808
  br i1 %t2043.not, label %L_632, label %L_631

L_629:                                            ; preds = %L_627
  %t2048 = load i8*, i8** %t14423, align 8
  %t2051 = load i64, i64* %t15180, align 4
  %t2052 = getelementptr inbounds i8, i8* %t2048, i64 %t2051
  br label %doSwitchNextBlock.backedge

L_628:                                            ; preds = %L_627
  %t2063 = add nsw i64 %t2061, -1
  br label %L_624

L_627:                                            ; preds = %L_622
  %t2059 = getelementptr inbounds i8, i8* %t2143, i64 -16
  %t2060 = bitcast i8* %t2059 to i64*
  %t2061 = load i64, i64* %t2060, align 4
  %t2066.not = icmp eq i64 %t2061, -9223372036854775808
  br i1 %t2066.not, label %L_629, label %L_628

L_626:                                            ; preds = %L_624
  %t2071 = load i8*, i8** %t14423, align 8
  %t2074 = load i64, i64* %t15180, align 4
  %t2075 = getelementptr inbounds i8, i8* %t2071, i64 %t2074
  br label %doSwitchNextBlock.backedge

L_624:                                            ; preds = %L_622, %L_628
  %TW64_1.0 = phi i64 [ %t2063, %L_628 ], [ 1, %L_622 ]
  %t2086 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %TW64_1.0, i64 %TW64_0.2)
  %t2087 = extractvalue { i64, i1 } %t2086, 1
  br i1 %t2087, label %L_626, label %doSwitchNextBlock.backedge

L_622:                                            ; preds = %L_620, %L_631
  %TW64_0.2 = phi i64 [ %t2040, %L_631 ], [ 1, %L_620 ]
  %27 = and i64 %t2145, 1
  %.not1796.not = icmp eq i64 %27, 0
  br i1 %.not1796.not, label %L_627, label %L_624

L_620:                                            ; preds = %x_3, %L_617, %L_618
  %.not1793.not = icmp eq i64 %t2148, 0
  br i1 %.not1793.not, label %L_630, label %L_622

L_618:                                            ; preds = %L_617
  %t2110 = shl i64 %t2130, 1
  %t2115 = xor i64 %t2110, %t2130
  %trunc1800 = icmp sgt i64 %t2115, -1
  br i1 %trunc1800, label %doSwitchNextBlock.backedge, label %L_620

L_617:                                            ; preds = %x_3
  %t2123 = ashr i64 %t2147, 1
  %t2127 = ashr i64 %t2145, 1
  %t2130 = mul i64 %t2127, %t2123
  %t2133 = tail call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %t2127, i64 %t2123)
  %t2134 = extractvalue { i64, i1 } %t2133, 1
  br i1 %t2134, label %L_620, label %L_618

x_3:                                              ; preds = %L_616, %L_637
  %TP_0.6 = phi i8* [ %t1944, %L_637 ], [ %TP_0.7, %L_616 ]
  %stackTop.18 = phi i8* [ %stackTop.23, %L_637 ], [ %stackTop.19, %L_616 ]
  %frontier.20 = phi i8* [ %frontier.25, %L_637 ], [ %frontier.21, %L_616 ]
  %t2138 = getelementptr inbounds i8, i8* %stackTop.18, i64 8
  %t2139 = bitcast i8* %t2138 to i8**
  %t2140 = load i8*, i8** %t2139, align 8
  %t2141 = getelementptr inbounds i8, i8* %t2140, i64 8
  %t2142 = bitcast i8* %t2141 to i8**
  %t2143 = load i8*, i8** %t2142, align 8
  %t2145 = ptrtoint i8* %t2143 to i64
  %t2147 = ptrtoint i8* %TP_0.6 to i64
  %t2148 = and i64 %t2147, 1
  %28 = and i64 %t2148, %t2145
  %.not1791.not = icmp eq i64 %28, 0
  br i1 %.not1791.not, label %L_620, label %L_617

L_616:                                            ; preds = %L_613, %L_zeroLen_6, %L_nonZeroLen_6
  %TP_0.7 = phi i8* [ %t2226, %L_nonZeroLen_6 ], [ getelementptr (i8, i8* @staticHeapM, i64 40), %L_zeroLen_6 ], [ %TP_2.0.ph, %L_613 ]
  %stackTop.19 = phi i8* [ %t2207, %L_nonZeroLen_6 ], [ %stackTop.21, %L_zeroLen_6 ], [ %stackTop.20.ph, %L_613 ]
  %frontier.21 = phi i8* [ %t2229, %L_nonZeroLen_6 ], [ %frontier.23, %L_zeroLen_6 ], [ %frontier.22.ph, %L_613 ]
  %t2156 = getelementptr inbounds i8, i8* %TP_0.7, i64 -8
  %t2157 = bitcast i8* %t2156 to i64*
  store i64 17, i64* %t2157, align 4
  br label %x_3

L_613:                                            ; preds = %L_613, %L_613.preheader
  %TP_1.0.in.in = phi i8* [ %TP_0.8, %L_613 ], [ %TP_1.0.in.in.ph, %L_613.preheader ]
  %TP_0.8.in.in = getelementptr inbounds i8, i8* %TP_1.0.in.in, i64 8
  %TP_0.8.in = bitcast i8* %TP_0.8.in.in to i8**
  %TP_0.8 = load i8*, i8** %TP_0.8.in, align 8
  %TP_1.0.in = bitcast i8* %TP_1.0.in.in to i8**
  %TP_1.0 = load i8*, i8** %TP_1.0.in, align 8
  %t2173 = bitcast i8* %TP_1.0 to i64*
  %t2174 = load i64, i64* %t2173, align 4
  %t2176 = getelementptr inbounds i8, i8* %TP_1.0, i64 8
  %t2177 = bitcast i8* %t2176 to i64*
  %t2178 = load i64, i64* %t2177, align 4
  %t2181 = shl nsw i64 %t2178, 3
  %t2182 = getelementptr inbounds i8, i8* %TP_2.0.ph, i64 %t2181
  %t2184 = bitcast i8* %t2182 to i64*
  store i64 %t2174, i64* %t2184, align 4
  %cond68 = icmp eq i8* %TP_0.8, inttoptr (i64 1 to i8*)
  br i1 %cond68, label %L_616, label %L_613

L_613.preheader:                                  ; preds = %L_nonZeroLen_6, %L_zeroLen_6
  %TP_2.0.ph = phi i8* [ getelementptr (i8, i8* @staticHeapM, i64 40), %L_zeroLen_6 ], [ %t2226, %L_nonZeroLen_6 ]
  %TP_1.0.in.in.ph = phi i8* [ %TP_0.9, %L_zeroLen_6 ], [ %t2212, %L_nonZeroLen_6 ]
  %stackTop.20.ph = phi i8* [ %stackTop.21, %L_zeroLen_6 ], [ %t2207, %L_nonZeroLen_6 ]
  %frontier.22.ph = phi i8* [ %frontier.23, %L_zeroLen_6 ], [ %t2229, %L_nonZeroLen_6 ]
  br label %L_613

L_nonZeroLen_6:                                   ; preds = %L_1811
  %t2216 = getelementptr inbounds i8, i8* %stackTop.21, i64 16
  %t2217 = bitcast i8* %t2216 to i64*
  store i64 149, i64* %t2217, align 4
  %t2219 = getelementptr inbounds i8, i8* %stackTop.21, i64 24
  store i8* %frontier.23, i8** %t15197, align 8
  store i8* %t2219, i8** %t15200, align 8
  %t2226 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 %TW64_0.3, i64 53)
  %t2229 = load i8*, i8** %t15197, align 8
  %t2232 = load i8*, i8** %t15200, align 8
  %t2207 = getelementptr inbounds i8, i8* %t2232, i64 -24
  %t2211 = bitcast i8* %t2207 to i8**
  %t2212 = load i8*, i8** %t2211, align 8
  %cond69 = icmp eq i8* %t2212, inttoptr (i64 1 to i8*)
  br i1 %cond69, label %L_616, label %L_613.preheader

L_1811:                                           ; preds = %L_606
  %t2236 = bitcast i8* %stackTop.21 to i8**
  store i8* %TP_0.9, i8** %t2236, align 8
  %t2239.not = icmp eq i64 %TW64_0.3, 0
  br i1 %t2239.not, label %L_zeroLen_6, label %L_nonZeroLen_6

L_609:                                            ; preds = %L_607
  %t2244 = load i8*, i8** %t14423, align 8
  %t2247 = load i64, i64* %t15180, align 4
  %t2248 = getelementptr inbounds i8, i8* %t2244, i64 %t2247
  br label %doSwitchNextBlock.backedge

L_608:                                            ; preds = %L_607
  %t2290 = add nsw i64 %TW64_0.3, 1
  %t2254 = getelementptr inbounds i8, i8* %frontier.23, i64 8
  %t2259 = bitcast i8* %frontier.23 to i64*
  store i64 29, i64* %t2259, align 4
  %t2264 = bitcast i8* %t2254 to i64*
  store i64 %TW64_1.1, i64* %t2264, align 4
  %t2267 = getelementptr inbounds i8, i8* %frontier.23, i64 16
  %t2268 = bitcast i8* %t2267 to i64*
  store i64 %TW64_0.3, i64* %t2268, align 4
  %t2271 = getelementptr inbounds i8, i8* %frontier.23, i64 32
  %t2275 = getelementptr inbounds i8, i8* %frontier.23, i64 24
  %t2276 = bitcast i8* %t2275 to i64*
  store i64 97, i64* %t2276, align 4
  %t2278 = getelementptr inbounds i8, i8* %frontier.23, i64 48
  %t2281 = bitcast i8* %t2271 to i8**
  store i8* %t2254, i8** %t2281, align 8
  %t2284 = getelementptr inbounds i8, i8* %frontier.23, i64 40
  %t2285 = bitcast i8* %t2284 to i8**
  store i8* %TP_0.9, i8** %t2285, align 8
  br label %loop_21

L_607:                                            ; preds = %L_606
  %t2293.not = icmp eq i64 %TW64_0.3, 9223372036854775807
  br i1 %t2293.not, label %L_609, label %L_608

L_606:                                            ; preds = %loop_21, %L_1812
  %TP_0.9 = phi i8* [ %t1966, %L_1812 ], [ %TP_0.10, %loop_21 ]
  %TW64_0.3 = phi i64 [ %t1962, %L_1812 ], [ %TW64_0.4, %loop_21 ]
  %TW64_1.1 = phi i64 [ %t1958, %L_1812 ], [ %TW64_1.2, %loop_21 ]
  %stackTop.21 = phi i8* [ %t1954, %L_1812 ], [ %stackTop.22, %loop_21 ]
  %frontier.23 = phi i8* [ %t1996, %L_1812 ], [ %frontier.24, %loop_21 ]
  %t2297.not = icmp eq i64 %TW64_1.1, 0
  br i1 %t2297.not, label %L_1811, label %L_607

loop_21:                                          ; preds = %L_602, %L_608
  %TP_0.10 = phi i8* [ %t2271, %L_608 ], [ %TP_0.10.ph, %L_602 ]
  %TW64_0.4 = phi i64 [ %t2290, %L_608 ], [ 1, %L_602 ]
  %TW64_1.2 = phi i64 [ 0, %L_608 ], [ %TW64_1.2.ph, %L_602 ]
  %stackTop.22 = phi i8* [ %stackTop.21, %L_608 ], [ %stackTop.23, %L_602 ]
  %frontier.24 = phi i8* [ %t2278, %L_608 ], [ %frontier.25, %L_602 ]
  %t2302 = load i8*, i8** %t14203, align 8
  %t2304.not = icmp ult i8* %t2302, %frontier.24
  br i1 %t2304.not, label %L_1812, label %L_606

L_602:                                            ; preds = %L_600
  %trunc1802 = icmp sgt i64 %t2327, -1
  %t1949 = sub i64 0, %t2327
  %TP_0.10.ph = select i1 %trunc1802, i8* getelementptr (i8, i8* @staticHeapI, i64 8920), i8* getelementptr (i8, i8* @staticHeapI, i64 8944)
  %TW64_1.2.ph = select i1 %trunc1802, i64 %t2327, i64 %t1949
  br label %loop_21

L_600:                                            ; preds = %L_597, %L_598
  %stackTop.23 = phi i8* [ %t1916, %L_598 ], [ %stackTop.24, %L_597 ]
  %frontier.25 = phi i8* [ %t1931, %L_598 ], [ %frontier.26, %L_597 ]
  %t2326 = bitcast i8* %stackTop.23 to i64*
  %t2327 = load i64, i64* %t2326, align 4
  %29 = add i64 %t2327, -4611686018427387904
  %30 = icmp sgt i64 %29, -1
  br i1 %30, label %L_602, label %L_637

L_597.sink.split:                                 ; preds = %L_871, %L_1699, %L_1700, %L_919, %L_1706, %L_1707, %L_946, %L_1710, %L_1711
  %t13064.sink = phi i8* [ %t13064, %L_1711 ], [ %t13084, %L_1710 ], [ %t13107, %L_946 ], [ %t13230, %L_1707 ], [ %t13250, %L_1706 ], [ %t13273, %L_919 ], [ %t13851, %L_1700 ], [ %t13871, %L_1699 ], [ %t13894, %L_871 ]
  %.sink5211 = phi i64 [ 68, %L_1711 ], [ 69, %L_1710 ], [ 70, %L_946 ], [ 72, %L_1707 ], [ 73, %L_1706 ], [ 74, %L_919 ], [ 76, %L_1700 ], [ 77, %L_1699 ], [ 78, %L_871 ]
  %stackTop.24.ph = phi i8* [ %stackTop.0, %L_1711 ], [ %stackTop.0, %L_1710 ], [ %t13096, %L_946 ], [ %stackTop.0, %L_1707 ], [ %stackTop.0, %L_1706 ], [ %t13262, %L_919 ], [ %stackTop.0, %L_1700 ], [ %stackTop.0, %L_1699 ], [ %t13883, %L_871 ]
  %frontier.26.ph = phi i8* [ %frontier.0, %L_1711 ], [ %frontier.0, %L_1710 ], [ %frontier.0, %L_946 ], [ %frontier.0, %L_1707 ], [ %frontier.0, %L_1706 ], [ %t13425, %L_919 ], [ %frontier.0, %L_1700 ], [ %frontier.0, %L_1699 ], [ %t13961, %L_871 ]
  %t13065 = bitcast i8* %t13064.sink to i64*
  store i64 %.sink5211, i64* %t13065, align 4
  br label %L_597

L_597:                                            ; preds = %L_597.sink.split, %doSwitchNextBlock
  %stackTop.24 = phi i8* [ %stackTop.0, %doSwitchNextBlock ], [ %stackTop.24.ph, %L_597.sink.split ]
  %frontier.26 = phi i8* [ %frontier.0, %doSwitchNextBlock ], [ %frontier.26.ph, %L_597.sink.split ]
  %t2336 = load i8*, i8** %t15210, align 8
  %t2338.not = icmp ult i8* %t2336, %stackTop.24
  br i1 %t2338.not, label %L_598, label %L_600

L_640:                                            ; preds = %L_638, %L_639
  %t2344 = getelementptr inbounds i8, i8* %stackTop.41, i64 24
  %t2345 = bitcast i8* %t2344 to i64*
  store i64 148, i64* %t2345, align 4
  %t2347 = getelementptr inbounds i8, i8* %stackTop.41, i64 32
  store i8* %frontier.44, i8** %t15197, align 8
  store i8* %t2347, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t2357 = load i8*, i8** %t15197, align 8
  %t2360 = load i8*, i8** %t15200, align 8
  %t2342 = getelementptr inbounds i8, i8* %t2360, i64 -32
  br label %L_642

L_748:                                            ; preds = %L_642
  %t2363 = load i8*, i8** %t14423, align 8
  %t2366 = load i64, i64* %t15180, align 4
  %t2367 = getelementptr inbounds i8, i8* %t2363, i64 %t2366
  %t2370 = bitcast i8* %t2367 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 9088), i8** %t2370, align 8
  %t2376 = load i8*, i8** %t14423, align 8
  %t2379 = load i64, i64* %t15180, align 4
  %t2380 = getelementptr inbounds i8, i8* %t2376, i64 %t2379
  br label %doSwitchNextBlock.backedge

L_747:                                            ; preds = %L_644
  %t2387 = shl nsw i64 %t3525, 1
  %t2389 = or i64 %t2387, 1
  %t2391 = getelementptr inbounds i8, i8* %stackTop.40, i64 32
  %t2392 = bitcast i8* %t2391 to i8**
  %t2394 = inttoptr i64 %t2389 to i8*
  store i8* %t2394, i8** %t2392, align 8
  br label %x_4

L_1809:                                           ; preds = %loop_22
  %t2417 = getelementptr inbounds i8, i8* %stackTop.39, i64 32
  %t2418 = bitcast i8* %t2417 to i64*
  store i64 %TW64_1.9, i64* %t2418, align 4
  %t2421 = getelementptr inbounds i8, i8* %stackTop.39, i64 40
  %t2422 = bitcast i8* %t2421 to i64*
  store i64 %TW64_0.10, i64* %t2422, align 4
  %t2425 = getelementptr inbounds i8, i8* %stackTop.39, i64 48
  %t2426 = bitcast i8* %t2425 to i8**
  store i8* %TP_0.18, i8** %t2426, align 8
  %t2429 = getelementptr inbounds i8, i8* %stackTop.39, i64 56
  %t2430 = bitcast i8* %t2429 to i64*
  store i64 147, i64* %t2430, align 4
  %t2432 = getelementptr inbounds i8, i8* %stackTop.39, i64 64
  store i8* %frontier.42, i8** %t15197, align 8
  store i8* %t2432, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t2442 = load i8*, i8** %t15197, align 8
  %t2445 = load i8*, i8** %t15200, align 8
  %t2400 = getelementptr inbounds i8, i8* %t2445, i64 -64
  %t2402 = getelementptr inbounds i8, i8* %t2445, i64 -32
  %t2403 = bitcast i8* %t2402 to i64*
  %t2404 = load i64, i64* %t2403, align 4
  %t2406 = getelementptr inbounds i8, i8* %t2445, i64 -24
  %t2407 = bitcast i8* %t2406 to i64*
  %t2408 = load i64, i64* %t2407, align 4
  %t2410 = getelementptr inbounds i8, i8* %t2445, i64 -16
  %t2411 = bitcast i8* %t2410 to i8**
  %t2412 = load i8*, i8** %t2411, align 8
  br label %L_650

L_zeroLen_8:                                      ; preds = %L_1805
  %cond64 = icmp eq i8* %TP_0.17, inttoptr (i64 1 to i8*)
  br i1 %cond64, label %L_660, label %L_657.preheader

L_742:                                            ; preds = %L_741
  %t2478 = add nsw i64 %t2476, -1
  br label %L_728

L_741:                                            ; preds = %L_726
  %t2474 = getelementptr inbounds i8, i8* %t3336, i64 -16
  %t2475 = bitcast i8* %t2474 to i64*
  %t2476 = load i64, i64* %t2475, align 4
  %t2481.not = icmp eq i64 %t2476, -9223372036854775808
  br i1 %t2481.not, label %L_653, label %L_742

L_740:                                            ; preds = %L_739
  %t2493 = add nsw i64 %t2491, -1
  br label %L_730

L_739:                                            ; preds = %L_728
  %t2489 = getelementptr inbounds i8, i8* %t3327, i64 -16
  %t2490 = bitcast i8* %t2489 to i64*
  %t2491 = load i64, i64* %t2490, align 4
  %t2496.not = icmp eq i64 %t2491, -9223372036854775808
  br i1 %t2496.not, label %L_653, label %L_740

L_734:                                            ; preds = %L_737
  %t2506 = getelementptr inbounds i8, i8* %stackTop.35, i64 48
  %t2507 = bitcast i8* %t2506 to i64*
  store i64 146, i64* %t2507, align 4
  %t2509 = getelementptr inbounds i8, i8* %stackTop.35, i64 56
  store i8* %frontier.37, i8** %t15197, align 8
  store i8* %t2509, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 %t2569, i32 0)
  %t2519 = load i8*, i8** %t15197, align 8
  %t2522 = load i8*, i8** %t15200, align 8
  %t2500 = getelementptr inbounds i8, i8* %t2522, i64 -56
  %t2529.phi.trans.insert = getelementptr inbounds i8, i8* %t2522, i64 -16
  %t2530.phi.trans.insert = bitcast i8* %t2529.phi.trans.insert to i8**
  %t2531.pre = load i8*, i8** %t2530.phi.trans.insert, align 8
  %t2533.phi.trans.insert = getelementptr inbounds i8, i8* %t2522, i64 -24
  %t2534.phi.trans.insert = bitcast i8* %t2533.phi.trans.insert to i8**
  %t2535.pre = load i8*, i8** %t2534.phi.trans.insert, align 8
  %t2537.phi.trans.insert = getelementptr inbounds i8, i8* %t2522, i64 -40
  %t2538.phi.trans.insert = bitcast i8* %t2537.phi.trans.insert to i64*
  %t2539.pre = load i64, i64* %t2538.phi.trans.insert, align 4
  %.pre4492 = bitcast i8* %t2537.phi.trans.insert to i8**
  br label %L_736

L_736:                                            ; preds = %L_737, %L_734
  %t2526.pre-phi = phi i8** [ %t3313, %L_737 ], [ %.pre4492, %L_734 ]
  %t2539 = phi i64 [ %t2569, %L_737 ], [ %t2539.pre, %L_734 ]
  %t2535 = phi i8* [ %t3336, %L_737 ], [ %t2535.pre, %L_734 ]
  %t2531 = phi i8* [ %t3327, %L_737 ], [ %t2531.pre, %L_734 ]
  %stackTop.25 = phi i8* [ %stackTop.35, %L_737 ], [ %t2500, %L_734 ]
  %frontier.27 = phi i8* [ %frontier.37, %L_737 ], [ %t2519, %L_734 ]
  store i8* %frontier.27, i8** %t15197, align 8
  %t2543 = tail call i8* @IntInf_mul(i8* nonnull %gcState, i8* %t2531, i8* %t2535, i64 %t2539)
  %t2546 = load i8*, i8** %t15197, align 8
  store i8* %t2543, i8** %t2526.pre-phi, align 8
  br label %L_664

L_737:                                            ; preds = %L_730
  %t2575 = add i64 %TW64_2.1, %TW64_1.3
  %t2564 = mul i64 %t2575, %t3317
  %t2566 = add i64 %t3317, 31
  %t2569 = add i64 %t2566, %t2564
  %t2572 = bitcast i8* %t3312 to i64*
  store i64 %t2569, i64* %t2572, align 4
  %t2549 = load i8*, i8** %t8218, align 8
  %t2551 = ptrtoint i8* %t2549 to i64
  %t2552 = ptrtoint i8* %frontier.37 to i64
  %t2553 = sub i64 %t2551, %t2552
  %t2559.not = icmp ult i64 %t2553, %t2569
  br i1 %t2559.not, label %L_734, label %L_736

L_730:                                            ; preds = %L_728, %L_740
  %TW64_2.1 = phi i64 [ %t2493, %L_740 ], [ 1, %L_728 ]
  %t2578 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %TW64_2.1, i64 %TW64_1.3)
  %t2579 = extractvalue { i64, i1 } %t2578, 1
  br i1 %t2579, label %L_653, label %L_737

L_728:                                            ; preds = %L_726, %L_742
  %TW64_1.3 = phi i64 [ %t2478, %L_742 ], [ 1, %L_726 ]
  %.not1722.not = icmp eq i64 %t3338, 0
  br i1 %.not1722.not, label %L_739, label %L_730

L_726:                                            ; preds = %x_4, %L_661, %L_662
  %31 = and i64 %t3337, 1
  %.not1719.not = icmp eq i64 %31, 0
  br i1 %.not1719.not, label %L_741, label %L_728

L_725:                                            ; preds = %L_664
  %t3259 = bitcast i8* %t3258 to i8**
  %t2604 = shl nsw i64 %t3263, 1
  %t2606 = or i64 %t2604, 1
  %t2611 = inttoptr i64 %t2606 to i8*
  store i8* %t2611, i8** %t3259, align 8
  br label %x_5

L_1808:                                           ; preds = %loop_23
  %t2634 = getelementptr inbounds i8, i8* %stackTop.33, i64 24
  %t2635 = bitcast i8* %t2634 to i64*
  store i64 %TW64_1.7, i64* %t2635, align 4
  %t2638 = getelementptr inbounds i8, i8* %stackTop.33, i64 32
  %t2639 = bitcast i8* %t2638 to i64*
  store i64 %TW64_0.8, i64* %t2639, align 4
  %t2642 = getelementptr inbounds i8, i8* %stackTop.33, i64 40
  %t2643 = bitcast i8* %t2642 to i8**
  store i8* %TP_0.14, i8** %t2643, align 8
  %t2646 = getelementptr inbounds i8, i8* %stackTop.33, i64 48
  %t2647 = bitcast i8* %t2646 to i64*
  store i64 145, i64* %t2647, align 4
  %t2649 = getelementptr inbounds i8, i8* %stackTop.33, i64 56
  store i8* %frontier.35, i8** %t15197, align 8
  store i8* %t2649, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t2659 = load i8*, i8** %t15197, align 8
  %t2662 = load i8*, i8** %t15200, align 8
  %t2617 = getelementptr inbounds i8, i8* %t2662, i64 -56
  %t2619 = getelementptr inbounds i8, i8* %t2662, i64 -32
  %t2620 = bitcast i8* %t2619 to i64*
  %t2621 = load i64, i64* %t2620, align 4
  %t2623 = getelementptr inbounds i8, i8* %t2662, i64 -24
  %t2624 = bitcast i8* %t2623 to i64*
  %t2625 = load i64, i64* %t2624, align 4
  %t2627 = getelementptr inbounds i8, i8* %t2662, i64 -16
  %t2628 = bitcast i8* %t2627 to i8**
  %t2629 = load i8*, i8** %t2628, align 8
  br label %L_670

L_zeroLen_7:                                      ; preds = %L_1807
  %cond61 = icmp eq i8* %TP_0.13, inttoptr (i64 1 to i8*)
  br i1 %cond61, label %L_679, label %L_676.preheader

L_720:                                            ; preds = %L_719
  %t2695 = add nsw i64 %t2693, -1
  br label %L_707

L_719:                                            ; preds = %L_705
  %t2691 = getelementptr inbounds i8, i8* %t3093, i64 -16
  %t2692 = bitcast i8* %t2691 to i64*
  %t2693 = load i64, i64* %t2692, align 4
  %t2698.not = icmp eq i64 %t2693, -9223372036854775808
  br i1 %t2698.not, label %L_653, label %L_720

L_718:                                            ; preds = %L_717
  %t2710 = add nsw i64 %t2708, -1
  br label %L_709

L_717:                                            ; preds = %L_707
  %t2706 = getelementptr inbounds i8, i8* %t3084, i64 -16
  %t2707 = bitcast i8* %t2706 to i64*
  %t2708 = load i64, i64* %t2707, align 4
  %t2713.not = icmp eq i64 %t2708, -9223372036854775808
  br i1 %t2713.not, label %L_653, label %L_718

L_712:                                            ; preds = %L_715
  %t2723 = getelementptr inbounds i8, i8* %stackTop.29, i64 40
  %t2724 = bitcast i8* %t2723 to i64*
  store i64 144, i64* %t2724, align 4
  %t2726 = getelementptr inbounds i8, i8* %stackTop.29, i64 48
  store i8* %frontier.31, i8** %t15197, align 8
  store i8* %t2726, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 %t2786, i32 0)
  %t2736 = load i8*, i8** %t15197, align 8
  %t2739 = load i8*, i8** %t15200, align 8
  %t2717 = getelementptr inbounds i8, i8* %t2739, i64 -48
  %t2746.phi.trans.insert = getelementptr inbounds i8, i8* %t2739, i64 -16
  %t2747.phi.trans.insert = bitcast i8* %t2746.phi.trans.insert to i8**
  %t2748.pre = load i8*, i8** %t2747.phi.trans.insert, align 8
  %t2750.phi.trans.insert = getelementptr inbounds i8, i8* %t2739, i64 -24
  %t2751.phi.trans.insert = bitcast i8* %t2750.phi.trans.insert to i8**
  %t2752.pre = load i8*, i8** %t2751.phi.trans.insert, align 8
  %t2754.phi.trans.insert = getelementptr inbounds i8, i8* %t2739, i64 -40
  %t2755.phi.trans.insert = bitcast i8* %t2754.phi.trans.insert to i64*
  %t2756.pre = load i64, i64* %t2755.phi.trans.insert, align 4
  %.pre4493 = bitcast i8* %t2754.phi.trans.insert to i8**
  br label %L_714

L_714:                                            ; preds = %L_715, %L_712
  %t2743.pre-phi = phi i8** [ %t3070, %L_715 ], [ %.pre4493, %L_712 ]
  %t2756 = phi i64 [ %t2786, %L_715 ], [ %t2756.pre, %L_712 ]
  %t2752 = phi i8* [ %t3093, %L_715 ], [ %t2752.pre, %L_712 ]
  %t2748 = phi i8* [ %t3084, %L_715 ], [ %t2748.pre, %L_712 ]
  %stackTop.26 = phi i8* [ %stackTop.29, %L_715 ], [ %t2717, %L_712 ]
  %frontier.28 = phi i8* [ %frontier.31, %L_715 ], [ %t2736, %L_712 ]
  store i8* %frontier.28, i8** %t15197, align 8
  %t2760 = tail call i8* @IntInf_mul(i8* nonnull %gcState, i8* %t2748, i8* %t2752, i64 %t2756)
  %t2763 = load i8*, i8** %t15197, align 8
  store i8* %t2760, i8** %t2743.pre-phi, align 8
  br label %L_683

L_715:                                            ; preds = %L_709
  %t2792 = add i64 %TW64_2.2, %TW64_1.4
  %t2781 = mul i64 %t2792, %t3074
  %t2783 = add i64 %t3074, 31
  %t2786 = add i64 %t2783, %t2781
  %t2789 = bitcast i8* %t3069 to i64*
  store i64 %t2786, i64* %t2789, align 4
  %t2766 = load i8*, i8** %t8218, align 8
  %t2768 = ptrtoint i8* %t2766 to i64
  %t2769 = ptrtoint i8* %frontier.31 to i64
  %t2770 = sub i64 %t2768, %t2769
  %t2776.not = icmp ult i64 %t2770, %t2786
  br i1 %t2776.not, label %L_712, label %L_714

L_709:                                            ; preds = %L_707, %L_718
  %TW64_2.2 = phi i64 [ %t2710, %L_718 ], [ 1, %L_707 ]
  %t2795 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %TW64_2.2, i64 %TW64_1.4)
  %t2796 = extractvalue { i64, i1 } %t2795, 1
  br i1 %t2796, label %L_653, label %L_715

L_707:                                            ; preds = %L_705, %L_720
  %TW64_1.4 = phi i64 [ %t2695, %L_720 ], [ 1, %L_705 ]
  %.not1734.not = icmp eq i64 %t3095, 0
  br i1 %.not1734.not, label %L_717, label %L_709

L_705:                                            ; preds = %x_5, %L_680, %L_681
  %32 = and i64 %t3094, 1
  %.not1731.not = icmp eq i64 %32, 0
  br i1 %.not1731.not, label %L_719, label %L_707

L_704:                                            ; preds = %L_703
  %t2828 = add nsw i64 %t2826, -1
  br label %L_689

L_703:                                            ; preds = %L_687
  %t2824 = getelementptr inbounds i8, i8* %t3019, i64 -16
  %t2825 = bitcast i8* %t2824 to i64*
  %t2826 = load i64, i64* %t2825, align 4
  %t2831.not = icmp eq i64 %t2826, -9223372036854775808
  br i1 %t2831.not, label %L_653, label %L_704

L_702:                                            ; preds = %L_701
  %t2843 = add nsw i64 %t2841, -1
  br label %L_691

L_701:                                            ; preds = %L_689
  %t2839 = getelementptr inbounds i8, i8* %t3014, i64 -16
  %t2840 = bitcast i8* %t2839 to i64*
  %t2841 = load i64, i64* %t2840, align 4
  %t2846.not = icmp eq i64 %t2841, -9223372036854775808
  br i1 %t2846.not, label %L_653, label %L_702

L_695:                                            ; preds = %L_691
  %t2859 = getelementptr inbounds i8, i8* %stackTop.28, i64 32
  %t2860 = bitcast i8* %t2859 to i64*
  store i64 143, i64* %t2860, align 4
  %t2862 = getelementptr inbounds i8, i8* %stackTop.28, i64 40
  store i8* %frontier.30, i8** %t15197, align 8
  store i8* %t2862, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 %t2938, i32 0)
  %t2872 = load i8*, i8** %t15197, align 8
  %t2875 = load i8*, i8** %t15200, align 8
  %t2853 = getelementptr inbounds i8, i8* %t2875, i64 -40
  %t2886.phi.trans.insert = getelementptr inbounds i8, i8* %t2875, i64 -32
  %t2887.phi.trans.insert = bitcast i8* %t2886.phi.trans.insert to i8**
  %t2888.pre = load i8*, i8** %t2887.phi.trans.insert, align 8
  %t2890.phi.trans.insert = getelementptr inbounds i8, i8* %t2875, i64 -24
  %t2891.phi.trans.insert = bitcast i8* %t2890.phi.trans.insert to i8**
  %t2892.pre = load i8*, i8** %t2891.phi.trans.insert, align 8
  %t2894.phi.trans.insert = getelementptr inbounds i8, i8* %t2875, i64 -16
  %t2895.phi.trans.insert = bitcast i8* %t2894.phi.trans.insert to i64*
  %t2896.pre = load i64, i64* %t2895.phi.trans.insert, align 4
  br label %L_697

L_697:                                            ; preds = %L_691, %L_695
  %t2896 = phi i64 [ %t2896.pre, %L_695 ], [ %t2938, %L_691 ]
  %t2892 = phi i8* [ %t2892.pre, %L_695 ], [ %t3019, %L_691 ]
  %t2888 = phi i8* [ %t2888.pre, %L_695 ], [ %t3014, %L_691 ]
  %stackTop.27 = phi i8* [ %t2853, %L_695 ], [ %stackTop.28, %L_691 ]
  %frontier.29 = phi i8* [ %t2872, %L_695 ], [ %frontier.30, %L_691 ]
  store i8* %frontier.29, i8** %t15197, align 8
  %t2900 = tail call i8* @IntInf_add(i8* nonnull %gcState, i8* %t2888, i8* %t2892, i64 %t2896)
  %t2903 = load i8*, i8** %t15197, align 8
  %t2879 = bitcast i8* %stackTop.27 to i8**
  store i8* %t2900, i8** %t2879, align 8
  br label %doSwitchNextBlock.backedge

L_691:                                            ; preds = %L_689, %L_702
  %TW64_1.5 = phi i64 [ %t2843, %L_702 ], [ 1, %L_689 ]
  %t2944.not = icmp slt i64 %TW64_0.6, %TW64_1.5
  %spec.select2259 = select i1 %t2944.not, i64 %TW64_1.5, i64 %TW64_0.6
  %t2922 = bitcast i8* %stackTop.28 to i64*
  %t2923 = load i64, i64* %t2922, align 4
  %reass.add2646 = add i64 %spec.select2259, 2
  %reass.mul2647 = mul i64 %reass.add2646, %t2923
  %t2938 = add i64 %reass.mul2647, 31
  %t2940 = getelementptr inbounds i8, i8* %stackTop.28, i64 24
  %t2941 = bitcast i8* %t2940 to i64*
  store i64 %t2938, i64* %t2941, align 4
  %t2906 = load i8*, i8** %t8218, align 8
  %t2908 = ptrtoint i8* %t2906 to i64
  %t2909 = ptrtoint i8* %frontier.30 to i64
  %t2910 = sub i64 %t2908, %t2909
  %t2916.not = icmp ult i64 %t2910, %t2938
  br i1 %t2916.not, label %L_695, label %L_697

L_689:                                            ; preds = %L_687, %L_704
  %TW64_0.6 = phi i64 [ %t2828, %L_704 ], [ 1, %L_687 ]
  %.not1744.not = icmp eq i64 %t3021, 0
  br i1 %.not1744.not, label %L_701, label %L_691

L_687:                                            ; preds = %L_683, %L_684
  %33 = and i64 %t3020, 1
  %.not1741.not = icmp eq i64 %33, 0
  br i1 %.not1741.not, label %L_703, label %L_689

L_686:                                            ; preds = %L_684
  %t2980 = or i64 %t2978, 1
  %t2969 = bitcast i8* %stackTop.28 to i8**
  %t2971 = inttoptr i64 %t2980 to i8*
  store i8* %t2971, i8** %t2969, align 8
  br label %doSwitchNextBlock.backedge

L_684:                                            ; preds = %L_683
  %t2994 = ashr i64 %t3020, 1
  %t3001 = ashr i64 %t3015, 1
  %t3004 = add nsw i64 %t2994, %t3001
  %t2978 = shl i64 %t3004, 1
  %t2983 = xor i64 %t2978, %t3004
  %trunc1748 = icmp sgt i64 %t2983, -1
  br i1 %trunc1748, label %L_686, label %L_687

L_683:                                            ; preds = %L_682, %L_714
  %t3014 = phi i8* [ %t2760, %L_714 ], [ %t3032, %L_682 ]
  %stackTop.28 = phi i8* [ %stackTop.26, %L_714 ], [ %stackTop.29, %L_682 ]
  %frontier.30 = phi i8* [ %t2763, %L_714 ], [ %frontier.31, %L_682 ]
  %t3015 = ptrtoint i8* %t3014 to i64
  %t3017 = getelementptr inbounds i8, i8* %stackTop.28, i64 16
  %t3018 = bitcast i8* %t3017 to i8**
  %t3019 = load i8*, i8** %t3018, align 8
  %t3020 = ptrtoint i8* %t3019 to i64
  %t3021 = and i64 %t3015, 1
  %34 = and i64 %t3021, %t3020
  %.not1739.not = icmp eq i64 %34, 0
  br i1 %.not1739.not, label %L_687, label %L_684

L_682:                                            ; preds = %L_681
  %t3037 = or i64 %t3035, 1
  %t3032 = inttoptr i64 %t3037 to i8*
  store i8* %t3032, i8** %t3070, align 8
  br label %L_683

L_681:                                            ; preds = %L_680
  %t3035 = shl i64 %t3061, 1
  %t3040 = xor i64 %t3035, %t3061
  %trunc1750 = icmp sgt i64 %t3040, -1
  br i1 %trunc1750, label %L_682, label %L_705

L_680:                                            ; preds = %x_5
  %t3051 = ashr i64 %t3094, 1
  %t3058 = ashr i64 %t3089, 1
  %t3061 = mul i64 %t3051, %t3058
  %t3064 = tail call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %t3058, i64 %t3051)
  %t3065 = extractvalue { i64, i1 } %t3064, 1
  br i1 %t3065, label %L_705, label %L_681

x_5:                                              ; preds = %L_679, %L_725
  %t3093 = phi i8* [ %t2611, %L_725 ], [ %TP_0.11, %L_679 ]
  %stackTop.29 = phi i8* [ %stackTop.34, %L_725 ], [ %stackTop.30, %L_679 ]
  %frontier.31 = phi i8* [ %frontier.36, %L_725 ], [ %frontier.32, %L_679 ]
  %t3069 = getelementptr inbounds i8, i8* %stackTop.29, i64 8
  %t3070 = bitcast i8* %t3069 to i8**
  %t3071 = load i8*, i8** %t3070, align 8
  %t3073 = bitcast i8* %t3071 to i64*
  %t3074 = load i64, i64* %t3073, align 4
  %t3076 = getelementptr inbounds i8, i8* %stackTop.29, i64 32
  %t3077 = bitcast i8* %t3076 to i8**
  %t3082 = getelementptr inbounds i8, i8* %t3071, i64 8
  %t3083 = bitcast i8* %t3082 to i8**
  %t3084 = load i8*, i8** %t3083, align 8
  store i8* %t3084, i8** %t3077, align 8
  %t3089 = ptrtoint i8* %t3084 to i64
  %t3094 = ptrtoint i8* %t3093 to i64
  %t3095 = and i64 %t3089, 1
  %35 = and i64 %t3095, %t3094
  %.not1729.not = icmp eq i64 %35, 0
  br i1 %.not1729.not, label %L_705, label %L_680

L_679:                                            ; preds = %L_676, %L_zeroLen_7, %L_nonZeroLen_8
  %TP_0.11 = phi i8* [ %t3176, %L_nonZeroLen_8 ], [ getelementptr (i8, i8* @staticHeapM, i64 40), %L_zeroLen_7 ], [ %TP_2.1.ph, %L_676 ]
  %stackTop.30 = phi i8* [ %t3157, %L_nonZeroLen_8 ], [ %stackTop.32, %L_zeroLen_7 ], [ %stackTop.31.ph, %L_676 ]
  %frontier.32 = phi i8* [ %t3179, %L_nonZeroLen_8 ], [ %frontier.34, %L_zeroLen_7 ], [ %frontier.33.ph, %L_676 ]
  %t3103 = getelementptr inbounds i8, i8* %TP_0.11, i64 -8
  %t3104 = bitcast i8* %t3103 to i64*
  store i64 17, i64* %t3104, align 4
  %t3106 = getelementptr inbounds i8, i8* %stackTop.30, i64 24
  %t3107 = bitcast i8* %t3106 to i8**
  store i8* %TP_0.11, i8** %t3107, align 8
  br label %x_5

L_676:                                            ; preds = %L_676, %L_676.preheader
  %TP_1.1.in.in = phi i8* [ %TP_0.12, %L_676 ], [ %TP_1.1.in.in.ph, %L_676.preheader ]
  %TP_0.12.in.in = getelementptr inbounds i8, i8* %TP_1.1.in.in, i64 8
  %TP_0.12.in = bitcast i8* %TP_0.12.in.in to i8**
  %TP_0.12 = load i8*, i8** %TP_0.12.in, align 8
  %TP_1.1.in = bitcast i8* %TP_1.1.in.in to i8**
  %TP_1.1 = load i8*, i8** %TP_1.1.in, align 8
  %t3123 = bitcast i8* %TP_1.1 to i64*
  %t3124 = load i64, i64* %t3123, align 4
  %t3126 = getelementptr inbounds i8, i8* %TP_1.1, i64 8
  %t3127 = bitcast i8* %t3126 to i64*
  %t3128 = load i64, i64* %t3127, align 4
  %t3131 = shl nsw i64 %t3128, 3
  %t3132 = getelementptr inbounds i8, i8* %TP_2.1.ph, i64 %t3131
  %t3134 = bitcast i8* %t3132 to i64*
  store i64 %t3124, i64* %t3134, align 4
  %cond62 = icmp eq i8* %TP_0.12, inttoptr (i64 1 to i8*)
  br i1 %cond62, label %L_679, label %L_676

L_676.preheader:                                  ; preds = %L_nonZeroLen_8, %L_zeroLen_7
  %TP_2.1.ph = phi i8* [ getelementptr (i8, i8* @staticHeapM, i64 40), %L_zeroLen_7 ], [ %t3176, %L_nonZeroLen_8 ]
  %TP_1.1.in.in.ph = phi i8* [ %TP_0.13, %L_zeroLen_7 ], [ %t3162, %L_nonZeroLen_8 ]
  %stackTop.31.ph = phi i8* [ %stackTop.32, %L_zeroLen_7 ], [ %t3157, %L_nonZeroLen_8 ]
  %frontier.33.ph = phi i8* [ %frontier.34, %L_zeroLen_7 ], [ %t3179, %L_nonZeroLen_8 ]
  br label %L_676

L_nonZeroLen_8:                                   ; preds = %L_1807
  %t3166 = getelementptr inbounds i8, i8* %stackTop.32, i64 32
  %t3167 = bitcast i8* %t3166 to i64*
  store i64 142, i64* %t3167, align 4
  %t3169 = getelementptr inbounds i8, i8* %stackTop.32, i64 40
  store i8* %frontier.34, i8** %t15197, align 8
  store i8* %t3169, i8** %t15200, align 8
  %t3176 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 %TW64_0.7, i64 53)
  %t3179 = load i8*, i8** %t15197, align 8
  %t3182 = load i8*, i8** %t15200, align 8
  %t3157 = getelementptr inbounds i8, i8* %t3182, i64 -40
  %t3160 = getelementptr inbounds i8, i8* %t3182, i64 -16
  %t3161 = bitcast i8* %t3160 to i8**
  %t3162 = load i8*, i8** %t3161, align 8
  %cond63 = icmp eq i8* %t3162, inttoptr (i64 1 to i8*)
  br i1 %cond63, label %L_679, label %L_676.preheader

L_1807:                                           ; preds = %L_670
  %t3185 = getelementptr inbounds i8, i8* %stackTop.32, i64 24
  %t3186 = bitcast i8* %t3185 to i8**
  store i8* %TP_0.13, i8** %t3186, align 8
  %t3189.not = icmp eq i64 %TW64_0.7, 0
  br i1 %t3189.not, label %L_zeroLen_7, label %L_nonZeroLen_8

L_672:                                            ; preds = %L_671
  %t3229 = add nsw i64 %TW64_0.7, 1
  %t3193 = getelementptr inbounds i8, i8* %frontier.34, i64 8
  %t3198 = bitcast i8* %frontier.34 to i64*
  store i64 29, i64* %t3198, align 4
  %t3203 = bitcast i8* %t3193 to i64*
  store i64 %TW64_1.6, i64* %t3203, align 4
  %t3206 = getelementptr inbounds i8, i8* %frontier.34, i64 16
  %t3207 = bitcast i8* %t3206 to i64*
  store i64 %TW64_0.7, i64* %t3207, align 4
  %t3210 = getelementptr inbounds i8, i8* %frontier.34, i64 32
  %t3214 = getelementptr inbounds i8, i8* %frontier.34, i64 24
  %t3215 = bitcast i8* %t3214 to i64*
  store i64 97, i64* %t3215, align 4
  %t3217 = getelementptr inbounds i8, i8* %frontier.34, i64 48
  %t3220 = bitcast i8* %t3210 to i8**
  store i8* %t3193, i8** %t3220, align 8
  %t3223 = getelementptr inbounds i8, i8* %frontier.34, i64 40
  %t3224 = bitcast i8* %t3223 to i8**
  store i8* %TP_0.13, i8** %t3224, align 8
  br label %loop_23

L_671:                                            ; preds = %L_670
  %t3232.not = icmp eq i64 %TW64_0.7, 9223372036854775807
  br i1 %t3232.not, label %L_653, label %L_672

L_670:                                            ; preds = %loop_23, %L_1808
  %TP_0.13 = phi i8* [ %t2629, %L_1808 ], [ %TP_0.14, %loop_23 ]
  %TW64_0.7 = phi i64 [ %t2625, %L_1808 ], [ %TW64_0.8, %loop_23 ]
  %TW64_1.6 = phi i64 [ %t2621, %L_1808 ], [ %TW64_1.7, %loop_23 ]
  %stackTop.32 = phi i8* [ %t2617, %L_1808 ], [ %stackTop.33, %loop_23 ]
  %frontier.34 = phi i8* [ %t2659, %L_1808 ], [ %frontier.35, %loop_23 ]
  %t3236.not = icmp eq i64 %TW64_1.6, 0
  br i1 %t3236.not, label %L_1807, label %L_671

loop_23:                                          ; preds = %L_666, %L_672
  %TP_0.14 = phi i8* [ %t3210, %L_672 ], [ %TP_0.14.ph, %L_666 ]
  %TW64_0.8 = phi i64 [ %t3229, %L_672 ], [ 1, %L_666 ]
  %TW64_1.7 = phi i64 [ 0, %L_672 ], [ %TW64_1.7.ph, %L_666 ]
  %stackTop.33 = phi i8* [ %stackTop.32, %L_672 ], [ %stackTop.34, %L_666 ]
  %frontier.35 = phi i8* [ %t3217, %L_672 ], [ %frontier.36, %L_666 ]
  %t3241 = load i8*, i8** %t14203, align 8
  %t3243.not = icmp ult i8* %t3241, %frontier.35
  br i1 %t3243.not, label %L_1808, label %L_670

L_666:                                            ; preds = %L_664
  %trunc1752 = icmp sgt i64 %t3263, -1
  %t2613 = sub i64 0, %t3263
  %TP_0.14.ph = select i1 %trunc1752, i8* getelementptr (i8, i8* @staticHeapI, i64 8920), i8* getelementptr (i8, i8* @staticHeapI, i64 8944)
  %TW64_1.7.ph = select i1 %trunc1752, i64 %t3263, i64 %t2613
  br label %loop_23

L_664:                                            ; preds = %L_663, %L_736
  %stackTop.34 = phi i8* [ %stackTop.25, %L_736 ], [ %stackTop.35, %L_663 ]
  %frontier.36 = phi i8* [ %t2546, %L_736 ], [ %frontier.37, %L_663 ]
  %t3258 = getelementptr inbounds i8, i8* %stackTop.34, i64 24
  %36 = bitcast i8* %t3258 to i64**
  %t32601726 = load i64*, i64** %36, align 8
  %t3263 = load i64, i64* %t32601726, align 4
  %37 = add i64 %t3263, -4611686018427387904
  %38 = icmp sgt i64 %37, -1
  br i1 %38, label %L_666, label %L_725

L_663:                                            ; preds = %L_662
  %t3280 = or i64 %t3278, 1
  %t3275 = inttoptr i64 %t3280 to i8*
  store i8* %t3275, i8** %t3313, align 8
  br label %L_664

L_662:                                            ; preds = %L_661
  %t3278 = shl i64 %t3304, 1
  %t3283 = xor i64 %t3278, %t3304
  %trunc1758 = icmp sgt i64 %t3283, -1
  br i1 %trunc1758, label %L_663, label %L_726

L_661:                                            ; preds = %x_4
  %t3294 = ashr i64 %t3337, 1
  %t3301 = ashr i64 %t3332, 1
  %t3304 = mul i64 %t3294, %t3301
  %t3307 = tail call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %t3301, i64 %t3294)
  %t3308 = extractvalue { i64, i1 } %t3307, 1
  br i1 %t3308, label %L_726, label %L_662

x_4:                                              ; preds = %L_660, %L_747
  %t3336 = phi i8* [ %t2394, %L_747 ], [ %TP_0.15, %L_660 ]
  %stackTop.35 = phi i8* [ %stackTop.40, %L_747 ], [ %stackTop.36, %L_660 ]
  %frontier.37 = phi i8* [ %t3564, %L_747 ], [ %frontier.38, %L_660 ]
  %t3312 = getelementptr inbounds i8, i8* %stackTop.35, i64 16
  %t3313 = bitcast i8* %t3312 to i8**
  %t3314 = load i8*, i8** %t3313, align 8
  %t3316 = bitcast i8* %t3314 to i64*
  %t3317 = load i64, i64* %t3316, align 4
  %t3319 = getelementptr inbounds i8, i8* %stackTop.35, i64 40
  %t3320 = bitcast i8* %t3319 to i8**
  %t3325 = getelementptr inbounds i8, i8* %t3314, i64 8
  %t3326 = bitcast i8* %t3325 to i8**
  %t3327 = load i8*, i8** %t3326, align 8
  store i8* %t3327, i8** %t3320, align 8
  %t3332 = ptrtoint i8* %t3327 to i64
  %t3337 = ptrtoint i8* %t3336 to i64
  %t3338 = and i64 %t3332, 1
  %39 = and i64 %t3338, %t3337
  %.not1717.not = icmp eq i64 %39, 0
  br i1 %.not1717.not, label %L_726, label %L_661

L_660:                                            ; preds = %L_657, %L_zeroLen_8, %L_nonZeroLen_7
  %TP_0.15 = phi i8* [ %t3419, %L_nonZeroLen_7 ], [ getelementptr (i8, i8* @staticHeapM, i64 40), %L_zeroLen_8 ], [ %TP_2.2.ph, %L_657 ]
  %stackTop.36 = phi i8* [ %t3400, %L_nonZeroLen_7 ], [ %stackTop.38, %L_zeroLen_8 ], [ %stackTop.37.ph, %L_657 ]
  %frontier.38 = phi i8* [ %t3422, %L_nonZeroLen_7 ], [ %frontier.41, %L_zeroLen_8 ], [ %frontier.39.ph, %L_657 ]
  %t3346 = getelementptr inbounds i8, i8* %TP_0.15, i64 -8
  %t3347 = bitcast i8* %t3346 to i64*
  store i64 17, i64* %t3347, align 4
  %t3349 = getelementptr inbounds i8, i8* %stackTop.36, i64 32
  %t3350 = bitcast i8* %t3349 to i8**
  store i8* %TP_0.15, i8** %t3350, align 8
  br label %x_4

L_657:                                            ; preds = %L_657, %L_657.preheader
  %TP_1.2.in.in = phi i8* [ %TP_0.16, %L_657 ], [ %TP_1.2.in.in.ph, %L_657.preheader ]
  %TP_0.16.in.in = getelementptr inbounds i8, i8* %TP_1.2.in.in, i64 8
  %TP_0.16.in = bitcast i8* %TP_0.16.in.in to i8**
  %TP_0.16 = load i8*, i8** %TP_0.16.in, align 8
  %TP_1.2.in = bitcast i8* %TP_1.2.in.in to i8**
  %TP_1.2 = load i8*, i8** %TP_1.2.in, align 8
  %t3366 = bitcast i8* %TP_1.2 to i64*
  %t3367 = load i64, i64* %t3366, align 4
  %t3369 = getelementptr inbounds i8, i8* %TP_1.2, i64 8
  %t3370 = bitcast i8* %t3369 to i64*
  %t3371 = load i64, i64* %t3370, align 4
  %t3374 = shl nsw i64 %t3371, 3
  %t3375 = getelementptr inbounds i8, i8* %TP_2.2.ph, i64 %t3374
  %t3377 = bitcast i8* %t3375 to i64*
  store i64 %t3367, i64* %t3377, align 4
  %cond65 = icmp eq i8* %TP_0.16, inttoptr (i64 1 to i8*)
  br i1 %cond65, label %L_660, label %L_657

L_657.preheader:                                  ; preds = %L_nonZeroLen_7, %L_zeroLen_8
  %TP_2.2.ph = phi i8* [ getelementptr (i8, i8* @staticHeapM, i64 40), %L_zeroLen_8 ], [ %t3419, %L_nonZeroLen_7 ]
  %TP_1.2.in.in.ph = phi i8* [ %TP_0.17, %L_zeroLen_8 ], [ %t3405, %L_nonZeroLen_7 ]
  %stackTop.37.ph = phi i8* [ %stackTop.38, %L_zeroLen_8 ], [ %t3400, %L_nonZeroLen_7 ]
  %frontier.39.ph = phi i8* [ %frontier.41, %L_zeroLen_8 ], [ %t3422, %L_nonZeroLen_7 ]
  br label %L_657

L_nonZeroLen_7:                                   ; preds = %L_1805
  %t3409 = getelementptr inbounds i8, i8* %stackTop.38, i64 40
  %t3410 = bitcast i8* %t3409 to i64*
  store i64 141, i64* %t3410, align 4
  %t3412 = getelementptr inbounds i8, i8* %stackTop.38, i64 48
  store i8* %frontier.41, i8** %t15197, align 8
  store i8* %t3412, i8** %t15200, align 8
  %t3419 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 %TW64_0.9, i64 53)
  %t3422 = load i8*, i8** %t15197, align 8
  %t3425 = load i8*, i8** %t15200, align 8
  %t3400 = getelementptr inbounds i8, i8* %t3425, i64 -48
  %t3403 = getelementptr inbounds i8, i8* %t3425, i64 -16
  %t3404 = bitcast i8* %t3403 to i8**
  %t3405 = load i8*, i8** %t3404, align 8
  %cond66 = icmp eq i8* %t3405, inttoptr (i64 1 to i8*)
  br i1 %cond66, label %L_660, label %L_657.preheader

L_1805:                                           ; preds = %L_650
  %t3428 = getelementptr inbounds i8, i8* %stackTop.38, i64 32
  %t3429 = bitcast i8* %t3428 to i8**
  store i8* %TP_0.17, i8** %t3429, align 8
  %t3432.not = icmp eq i64 %TW64_0.9, 0
  br i1 %t3432.not, label %L_zeroLen_8, label %L_nonZeroLen_7

L_653:                                            ; preds = %L_651, %L_671, %L_701, %L_703, %L_709, %L_717, %L_719, %L_730, %L_739, %L_741
  %frontier.40 = phi i8* [ %frontier.37, %L_741 ], [ %frontier.37, %L_739 ], [ %frontier.37, %L_730 ], [ %frontier.31, %L_719 ], [ %frontier.31, %L_717 ], [ %frontier.31, %L_709 ], [ %frontier.30, %L_703 ], [ %frontier.30, %L_701 ], [ %frontier.34, %L_671 ], [ %frontier.41, %L_651 ]
  %t3437 = load i8*, i8** %t14423, align 8
  %t3440 = load i64, i64* %t15180, align 4
  %t3441 = getelementptr inbounds i8, i8* %t3437, i64 %t3440
  %t3444 = bitcast i8* %t3441 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t3444, align 8
  %t3448 = load i8*, i8** %t14423, align 8
  %t3451 = load i64, i64* %t15180, align 4
  %t3452 = getelementptr inbounds i8, i8* %t3448, i64 %t3451
  br label %doSwitchNextBlock.backedge

L_652:                                            ; preds = %L_651
  %t3494 = add nsw i64 %TW64_0.9, 1
  %t3458 = getelementptr inbounds i8, i8* %frontier.41, i64 8
  %t3463 = bitcast i8* %frontier.41 to i64*
  store i64 29, i64* %t3463, align 4
  %t3468 = bitcast i8* %t3458 to i64*
  store i64 %TW64_1.8, i64* %t3468, align 4
  %t3471 = getelementptr inbounds i8, i8* %frontier.41, i64 16
  %t3472 = bitcast i8* %t3471 to i64*
  store i64 %TW64_0.9, i64* %t3472, align 4
  %t3475 = getelementptr inbounds i8, i8* %frontier.41, i64 32
  %t3479 = getelementptr inbounds i8, i8* %frontier.41, i64 24
  %t3480 = bitcast i8* %t3479 to i64*
  store i64 97, i64* %t3480, align 4
  %t3482 = getelementptr inbounds i8, i8* %frontier.41, i64 48
  %t3485 = bitcast i8* %t3475 to i8**
  store i8* %t3458, i8** %t3485, align 8
  %t3488 = getelementptr inbounds i8, i8* %frontier.41, i64 40
  %t3489 = bitcast i8* %t3488 to i8**
  store i8* %TP_0.17, i8** %t3489, align 8
  br label %loop_22

L_651:                                            ; preds = %L_650
  %t3497.not = icmp eq i64 %TW64_0.9, 9223372036854775807
  br i1 %t3497.not, label %L_653, label %L_652

L_650:                                            ; preds = %loop_22, %L_1809
  %TP_0.17 = phi i8* [ %t2412, %L_1809 ], [ %TP_0.18, %loop_22 ]
  %TW64_0.9 = phi i64 [ %t2408, %L_1809 ], [ %TW64_0.10, %loop_22 ]
  %TW64_1.8 = phi i64 [ %t2404, %L_1809 ], [ %TW64_1.9, %loop_22 ]
  %stackTop.38 = phi i8* [ %t2400, %L_1809 ], [ %stackTop.39, %loop_22 ]
  %frontier.41 = phi i8* [ %t2442, %L_1809 ], [ %frontier.42, %loop_22 ]
  %t3501.not = icmp eq i64 %TW64_1.8, 0
  br i1 %t3501.not, label %L_1805, label %L_651

loop_22:                                          ; preds = %L_646, %L_652
  %TP_0.18 = phi i8* [ %t3475, %L_652 ], [ %TP_0.18.ph, %L_646 ]
  %TW64_0.10 = phi i64 [ %t3494, %L_652 ], [ 1, %L_646 ]
  %TW64_1.9 = phi i64 [ 0, %L_652 ], [ %TW64_1.9.ph, %L_646 ]
  %stackTop.39 = phi i8* [ %stackTop.38, %L_652 ], [ %stackTop.40, %L_646 ]
  %frontier.42 = phi i8* [ %t3482, %L_652 ], [ %t3564, %L_646 ]
  %t3506 = load i8*, i8** %t14203, align 8
  %t3508.not = icmp ult i8* %t3506, %frontier.42
  br i1 %t3508.not, label %L_1809, label %L_650

L_646:                                            ; preds = %L_644
  %trunc1760 = icmp sgt i64 %t3525, -1
  %t2396 = sub i64 0, %t3525
  %TP_0.18.ph = select i1 %trunc1760, i8* getelementptr (i8, i8* @staticHeapI, i64 8920), i8* getelementptr (i8, i8* @staticHeapI, i64 8944)
  %TW64_1.9.ph = select i1 %trunc1760, i64 %t3525, i64 %t2396
  br label %loop_22

L_644:                                            ; preds = %L_642
  %t3525 = load i64, i64* %t3549, align 4
  %40 = add i64 %t3525, -4611686018427387904
  %41 = icmp sgt i64 %40, -1
  br i1 %41, label %L_646, label %L_747

L_642:                                            ; preds = %L_639, %L_640
  %stackTop.40 = phi i8* [ %t2342, %L_640 ], [ %stackTop.41, %L_639 ]
  %frontier.43 = phi i8* [ %t2357, %L_640 ], [ %frontier.44, %L_639 ]
  %t3539 = getelementptr inbounds i8, i8* %frontier.43, i64 8
  %t3544 = bitcast i8* %frontier.43 to i64*
  store i64 41, i64* %t3544, align 4
  %t3549 = bitcast i8* %t3539 to i64*
  store i64 0, i64* %t3549, align 4
  %t3551 = getelementptr inbounds i8, i8* %frontier.43, i64 24
  %t3553 = getelementptr inbounds i8, i8* %stackTop.40, i64 24
  %t3554 = bitcast i8* %t3553 to i8**
  store i8* %t3551, i8** %t3554, align 8
  %t3561 = getelementptr inbounds i8, i8* %frontier.43, i64 16
  %t3562 = bitcast i8* %t3561 to i64*
  store i64 41, i64* %t3562, align 4
  %t3564 = getelementptr inbounds i8, i8* %frontier.43, i64 32
  %42 = bitcast i8* %t3553 to i64**
  %t35681713 = load i64*, i64** %42, align 8
  store i64 0, i64* %t35681713, align 4
  %t3575 = load i8*, i8** %t3554, align 8
  %t3576 = tail call i32 @Time_getTimeOfDay(i8* nonnull %t3539, i8* %t3575)
  %t3535.not = icmp eq i32 %t3576, -1
  br i1 %t3535.not, label %L_748, label %L_644

L_639:                                            ; preds = %L_638
  %t3579 = load i8*, i8** %t14203, align 8
  %t3581.not = icmp ult i8* %t3579, %frontier.44
  br i1 %t3581.not, label %L_640, label %L_642

L_638.sink.split:                                 ; preds = %L_1696, %L_1195, %L_1205
  %t9995.sink = phi i8* [ %t9995, %L_1205 ], [ %t10107, %L_1195 ], [ %t14236, %L_1696 ]
  %.sink5212 = phi i64 [ 64, %L_1205 ], [ 65, %L_1195 ], [ 79, %L_1696 ]
  %stackTop.41.ph = phi i8* [ %t9978, %L_1205 ], [ %t10090, %L_1195 ], [ %stackTop.0, %L_1696 ]
  %frontier.44.ph = phi i8* [ %frontier.101.lcssa4606, %L_1205 ], [ %frontier.102, %L_1195 ], [ %frontier.0, %L_1696 ]
  %t9996 = bitcast i8* %t9995.sink to i64*
  store i64 %.sink5212, i64* %t9996, align 4
  br label %L_638

L_638:                                            ; preds = %L_638.sink.split, %doSwitchNextBlock
  %stackTop.41 = phi i8* [ %stackTop.0, %doSwitchNextBlock ], [ %stackTop.41.ph, %L_638.sink.split ]
  %frontier.44 = phi i8* [ %frontier.0, %doSwitchNextBlock ], [ %frontier.44.ph, %L_638.sink.split ]
  %t3586 = load i8*, i8** %t15210, align 8
  %t3588.not = icmp ult i8* %t3586, %stackTop.41
  br i1 %t3588.not, label %L_640, label %L_639

L_751:                                            ; preds = %L_749, %L_750
  %t3594 = getelementptr inbounds i8, i8* %stackTop.45, i64 24
  %t3595 = bitcast i8* %t3594 to i64*
  store i64 140, i64* %t3595, align 4
  %t3597 = getelementptr inbounds i8, i8* %stackTop.45, i64 32
  store i8* %frontier.48, i8** %t15197, align 8
  store i8* %t3597, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t3607 = load i8*, i8** %t15197, align 8
  %t3610 = load i8*, i8** %t15200, align 8
  %t3592 = getelementptr inbounds i8, i8* %t3610, i64 -32
  br label %L_753

L_773:                                            ; preds = %L_753
  %t3616 = getelementptr inbounds i8, i8* %t3857, i64 -16
  %t3617 = bitcast i8* %t3616 to i64*
  %t3618 = load i64, i64* %t3617, align 4
  %t3623.not = icmp eq i64 %t3618, -9223372036854775808
  br i1 %t3623.not, label %L_1592.sink.split, label %L_760

common.ret:                                       ; preds = %print_1, %L_887, %L_932, %L_981, %L_1251, %L_1253, %L_1301, %L_764
  %common.ret.op = phi i64 [ %t3646, %L_764 ], [ 25, %L_1301 ], [ 11, %L_1253 ], [ 25, %L_1251 ], [ 26, %L_981 ], [ 32, %L_932 ], [ 32, %L_887 ], [ %t15050, %print_1 ]
  ret i64 %common.ret.op

L_764:                                            ; preds = %L_769
  tail call void @MLton_heapCheckTooLarge()
  %t3646 = tail call i64 @MLton_unreachable()
  br label %common.ret

L_766:                                            ; preds = %L_765
  %t3651 = getelementptr inbounds i8, i8* %stackTop.44, i64 32
  %t3652 = bitcast i8* %t3651 to i64*
  store i64 139, i64* %t3652, align 4
  %t3654 = getelementptr inbounds i8, i8* %stackTop.44, i64 40
  store i8* %frontier.47, i8** %t15197, align 8
  store i8* %t3654, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 %t3717, i32 0)
  %t3664 = load i8*, i8** %t15197, align 8
  %t3667 = load i8*, i8** %t15200, align 8
  %t3648 = getelementptr inbounds i8, i8* %t3667, i64 -40
  %t3672.phi.trans.insert = bitcast i8* %t3648 to i8**
  %t3673.pre = load i8*, i8** %t3672.phi.trans.insert, align 8
  %t3675.phi.trans.insert = getelementptr inbounds i8, i8* %t3667, i64 -16
  %t3676.phi.trans.insert = bitcast i8* %t3675.phi.trans.insert to i64*
  %t3677.pre = load i64, i64* %t3676.phi.trans.insert, align 4
  br label %L_768

L_768:                                            ; preds = %L_765, %L_766
  %t3672.pre-phi = phi i8** [ %t3856, %L_765 ], [ %t3672.phi.trans.insert, %L_766 ]
  %t3677 = phi i64 [ %t3709, %L_765 ], [ %t3677.pre, %L_766 ]
  %t3673 = phi i8* [ %t3857, %L_765 ], [ %t3673.pre, %L_766 ]
  %stackTop.42 = phi i8* [ %stackTop.44, %L_765 ], [ %t3648, %L_766 ]
  %frontier.45 = phi i8* [ %frontier.47, %L_765 ], [ %t3664, %L_766 ]
  store i8* %frontier.45, i8** %t15197, align 8
  %t3681 = tail call i8* @IntInf_quot(i8* nonnull %gcState, i8* nonnull inttoptr (i64 2000000001 to i8*), i8* %t3673, i64 %t3677)
  %t3684 = load i8*, i8** %t15197, align 8
  br label %L_756

L_765:                                            ; preds = %L_769
  %t3687 = load i8*, i8** %t8218, align 8
  %t3689 = ptrtoint i8* %t3687 to i64
  %t3690 = ptrtoint i8* %frontier.47 to i64
  %t3691 = sub i64 %t3689, %t3690
  %t3694.not = icmp ult i64 %t3691, %t3717
  br i1 %t3694.not, label %L_766, label %L_768

L_769:                                            ; preds = %L_762
  %reass.add2644 = sub i64 4, %t3618
  %reass.mul2645 = mul i64 %reass.add2644, %t3853
  %t3706 = add i64 %t3853, 31
  %t3709 = add i64 %t3706, %reass.mul2645
  %t3711 = getelementptr inbounds i8, i8* %stackTop.44, i64 24
  %t3712 = bitcast i8* %t3711 to i64*
  store i64 %t3709, i64* %t3712, align 4
  %t3717 = add i64 %t3709, 48
  %t3723 = icmp ult i64 %t3709, -48
  br i1 %t3723, label %L_765, label %L_764

L_762:                                            ; preds = %L_761
  %t3729 = tail call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 1, i64 %t3620)
  %t3730 = extractvalue { i64, i1 } %t3729, 1
  br i1 %t3730, label %L_1592.sink.split, label %L_769

L_761:                                            ; preds = %L_760
  %t3740.not = icmp eq i8* %t3857, inttoptr (i64 1 to i8*)
  br i1 %t3740.not, label %L_1592.sink.split, label %L_762

L_760:                                            ; preds = %L_773
  %t3620 = add nsw i64 %t3618, -1
  %t3744 = icmp slt i64 %t3618, 3
  br i1 %t3744, label %L_761, label %L_756

L_756:                                            ; preds = %L_760, %L_755, %L_768
  %t3817.pre-phi = phi i8** [ %t3856, %L_760 ], [ %t3856, %L_755 ], [ %t3672.pre-phi, %L_768 ]
  %TP_0.19 = phi i8* [ inttoptr (i64 1 to i8*), %L_760 ], [ %t3835, %L_755 ], [ %t3681, %L_768 ]
  %stackTop.43 = phi i8* [ %stackTop.44, %L_760 ], [ %stackTop.44, %L_755 ], [ %stackTop.42, %L_768 ]
  %frontier.46 = phi i8* [ %frontier.47, %L_760 ], [ %frontier.47, %L_755 ], [ %t3684, %L_768 ]
  %t3776 = getelementptr inbounds i8, i8* %frontier.46, i64 8
  %t3781 = bitcast i8* %frontier.46 to i64*
  store i64 67, i64* %t3781, align 4
  %t3786 = bitcast i8* %t3776 to i64*
  %t3788 = getelementptr inbounds i8, i8* %stackTop.43, i64 8
  %t3789 = bitcast i8* %t3788 to i64*
  %t3790 = load i64, i64* %t3789, align 4
  store i64 %t3790, i64* %t3786, align 4
  %t3792 = getelementptr inbounds i8, i8* %frontier.46, i64 16
  %t3793 = bitcast i8* %t3792 to i8**
  store i8* %TP_0.19, i8** %t3793, align 8
  %t3796 = getelementptr inbounds i8, i8* %frontier.46, i64 32
  %t3800 = getelementptr inbounds i8, i8* %frontier.46, i64 24
  %t3801 = bitcast i8* %t3800 to i64*
  store i64 65, i64* %t3801, align 4
  %t3803 = getelementptr inbounds i8, i8* %frontier.46, i64 48
  %t3806 = bitcast i8* %t3796 to i8**
  store i8* %TP_0.19, i8** %t3806, align 8
  %t3809 = getelementptr inbounds i8, i8* %frontier.46, i64 40
  %t3810 = bitcast i8* %t3809 to i8**
  %t3812 = getelementptr inbounds i8, i8* %stackTop.43, i64 16
  %t3813 = bitcast i8* %t3812 to i8**
  %t3814 = load i8*, i8** %t3813, align 8
  store i8* %t3814, i8** %t3810, align 8
  store i8* %t3796, i8** %t3817.pre-phi, align 8
  %t3821 = bitcast i8* %t3788 to i8**
  store i8* %t3776, i8** %t3821, align 8
  br label %doSwitchNextBlock.backedge

L_755:                                            ; preds = %L_754
  %t3842 = ashr i64 %t3858, 1
  %t3828 = sdiv i64 1000000000, %t3842
  %t3831 = shl nsw i64 %t3828, 1
  %t3833 = or i64 %t3831, 1
  %t3835 = inttoptr i64 %t3833 to i8*
  br label %L_756

L_754:                                            ; preds = %L_753
  %t3844.not = icmp ult i8* %t3857, inttoptr (i64 2 to i8*)
  br i1 %t3844.not, label %L_1592.sink.split, label %L_755

L_753:                                            ; preds = %L_750, %L_751
  %stackTop.44 = phi i8* [ %t3592, %L_751 ], [ %stackTop.45, %L_750 ]
  %frontier.47 = phi i8* [ %t3607, %L_751 ], [ %frontier.48, %L_750 ]
  %t3848 = getelementptr inbounds i8, i8* %stackTop.44, i64 16
  %43 = bitcast i8* %t3848 to i64**
  %t38501700 = load i64*, i64** %43, align 8
  %t3853 = load i64, i64* %t38501700, align 4
  %t3856 = bitcast i8* %stackTop.44 to i8**
  %t3857 = load i8*, i8** %t3856, align 8
  %t3858 = ptrtoint i8* %t3857 to i64
  %44 = and i64 %t3858, 1
  %.not.not = icmp eq i64 %44, 0
  br i1 %.not.not, label %L_773, label %L_754

L_750:                                            ; preds = %L_749
  %t3868 = load i8*, i8** %t14203, align 8
  %t3870.not = icmp ult i8* %t3868, %frontier.48
  br i1 %t3870.not, label %L_751, label %L_753

L_749.sink.split:                                 ; preds = %L_844, %L_1694, %L_1695
  %stackTop.0.sink = phi i8* [ %stackTop.0, %L_1695 ], [ %stackTop.0, %L_1694 ], [ %t14445, %L_844 ]
  %.sink5213 = phi i64 [ 80, %L_1695 ], [ 81, %L_1694 ], [ 82, %L_844 ]
  %stackTop.45.ph = phi i8* [ %t14246, %L_1695 ], [ %t14274, %L_1694 ], [ %t4258.pn, %L_844 ]
  %frontier.48.ph = phi i8* [ %frontier.0, %L_1695 ], [ %frontier.0, %L_1694 ], [ %t14385, %L_844 ]
  %t14265 = bitcast i8* %stackTop.0.sink to i64*
  store i64 %.sink5213, i64* %t14265, align 4
  br label %L_749

L_749:                                            ; preds = %L_749.sink.split, %doSwitchNextBlock
  %stackTop.45 = phi i8* [ %stackTop.0, %doSwitchNextBlock ], [ %stackTop.45.ph, %L_749.sink.split ]
  %frontier.48 = phi i8* [ %frontier.0, %doSwitchNextBlock ], [ %frontier.48.ph, %L_749.sink.split ]
  %t3875 = load i8*, i8** %t15210, align 8
  %t3877.not = icmp ult i8* %t3875, %stackTop.45
  br i1 %t3877.not, label %L_751, label %L_750

L_777:                                            ; preds = %L_1682, %L_776
  %t3883 = getelementptr inbounds i8, i8* %stackTop.148, i64 24
  %t3884 = bitcast i8* %t3883 to i64*
  store i64 94, i64* %t3884, align 4
  %t3886 = getelementptr inbounds i8, i8* %stackTop.148, i64 32
  store i8* %frontier.146, i8** %t15197, align 8
  store i8* %t3886, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t3896 = load i8*, i8** %t15197, align 8
  %t3899 = load i8*, i8** %t15200, align 8
  %t3881 = getelementptr inbounds i8, i8* %t3899, i64 -32
  br label %L_779

L_1803:                                           ; preds = %loop_24
  %t3918 = getelementptr inbounds i8, i8* %stackTop.146, i64 40
  %t3919 = bitcast i8* %t3918 to i64*
  store i64 %TW64_1.27, i64* %t3919, align 4
  %t3922 = getelementptr inbounds i8, i8* %stackTop.146, i64 48
  %t3923 = bitcast i8* %t3922 to i64*
  store i64 %TW64_0.43, i64* %t3923, align 4
  %t3926 = getelementptr inbounds i8, i8* %stackTop.146, i64 56
  %t3927 = bitcast i8* %t3926 to i8**
  store i8* %TP_0.76, i8** %t3927, align 8
  %t3930 = getelementptr inbounds i8, i8* %stackTop.146, i64 64
  %t3931 = bitcast i8* %t3930 to i64*
  store i64 138, i64* %t3931, align 4
  %t3933 = getelementptr inbounds i8, i8* %stackTop.146, i64 72
  store i8* %frontier.144, i8** %t15197, align 8
  store i8* %t3933, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t3943 = load i8*, i8** %t15197, align 8
  %t3946 = load i8*, i8** %t15200, align 8
  %t3901 = getelementptr inbounds i8, i8* %t3946, i64 -72
  %t3903 = getelementptr inbounds i8, i8* %t3946, i64 -32
  %t3904 = bitcast i8* %t3903 to i64*
  %t3905 = load i64, i64* %t3904, align 4
  %t3907 = getelementptr inbounds i8, i8* %t3946, i64 -24
  %t3908 = bitcast i8* %t3907 to i64*
  %t3909 = load i64, i64* %t3908, align 4
  %t3911 = getelementptr inbounds i8, i8* %t3946, i64 -16
  %t3912 = bitcast i8* %t3911 to i8**
  %t3913 = load i8*, i8** %t3912, align 8
  br label %L_782

L_1681:                                           ; preds = %L_zeroLen_17
  %t3949 = bitcast i8* %t15007 to i8**
  store i8* getelementptr (i8, i8* @staticHeapM, i64 40), i8** %t3949, align 8
  br label %L_815

L_zeroLen_17:                                     ; preds = %L_1684
  %cond57 = icmp eq i8* %TP_0.75, inttoptr (i64 1 to i8*)
  br i1 %cond57, label %L_1681, label %L_809.preheader

L_804:                                            ; preds = %L_nonZeroLen_9
  %t3976 = getelementptr inbounds i8, i8* %stackTop.145, i64 56
  %t3977 = bitcast i8* %t3976 to i64*
  store i64 85, i64* %t3977, align 4
  %t3979 = getelementptr inbounds i8, i8* %stackTop.145, i64 64
  store i8* %frontier.143, i8** %t15197, align 8
  store i8* %t3979, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t3989 = load i8*, i8** %t15197, align 8
  %t3992 = load i8*, i8** %t15200, align 8
  %t3974 = getelementptr inbounds i8, i8* %t3992, i64 -64
  %t14978.phi.trans.insert = getelementptr inbounds i8, i8* %t3992, i64 -24
  %t14979.phi.trans.insert = bitcast i8* %t14978.phi.trans.insert to i64*
  %t14980.pre = load i64, i64* %t14979.phi.trans.insert, align 4
  br label %L_806

L_1679:                                           ; preds = %L_806
  %t3994 = getelementptr inbounds i8, i8* %t14998, i64 -24
  %t3995 = bitcast i8* %t3994 to i8**
  store i8* %t14992, i8** %t3995, align 8
  br label %L_815

L_812:                                            ; preds = %L_1685
  %t4000 = getelementptr inbounds i8, i8* %stackTop.143.ph, i64 48
  %t4001 = bitcast i8* %t4000 to i64*
  store i64 137, i64* %t4001, align 4
  %t4003 = getelementptr inbounds i8, i8* %stackTop.143.ph, i64 56
  store i8* %frontier.141.ph, i8** %t15197, align 8
  store i8* %t4003, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4013 = load i8*, i8** %t15197, align 8
  %t4016 = load i8*, i8** %t15200, align 8
  %t3998 = getelementptr inbounds i8, i8* %t4016, i64 -56
  br label %L_815

L_1675:                                           ; preds = %L_1672, %L_1674
  %TW64_1.11 = phi i64 [ %t4044, %L_1674 ], [ 0, %L_1672 ]
  %t4026 = shl nsw i64 %TW64_0.393597, 3
  %t4027 = getelementptr inbounds i8, i8* %t14838, i64 %t4026
  %t4029 = bitcast i8* %t4027 to i64*
  store i64 %TW64_1.11, i64* %t4029, align 4
  %t4032 = add nuw nsw i64 %TW64_0.393597, 1
  %t14820 = icmp ugt i64 %TW64_0.393597, 254
  br i1 %t14820, label %L_1667.preheader, label %L_1672

L_1667.preheader:                                 ; preds = %L_1675
  %t40845737 = load i8, i8* getelementptr (i8, i8* @staticHeapI, i64 8608), align 1
  %t4087.not.not5738 = icmp eq i8 %t40845737, 0
  %or.cond5739 = or i1 %t4087.not.not5738, icmp eq (i8* getelementptr (i8, i8* @staticHeapI, i64 8696), i8* inttoptr (i64 1 to i8*))
  br i1 %or.cond5739, label %L_1669, label %L_1671

L_1674:                                           ; preds = %L_1672
  %45 = trunc i64 %TW64_0.393597 to i32
  %t4037 = add nsw i32 %45, -48
  %t4039 = zext i32 %t4037 to i64
  %t4042 = shl nuw nsw i64 %t4039, 1
  %t4044 = or i64 %t4042, 1
  br label %L_1675

L_1672:                                           ; preds = %L_815, %L_1675
  %TW64_0.393597 = phi i64 [ 0, %L_815 ], [ %t4032, %L_1675 ]
  %t4052 = trunc i64 %TW64_0.393597 to i8
  %46 = add i8 %t4052, -48
  %47 = icmp ult i8 %46, 10
  br i1 %47, label %L_1674, label %L_1675

L_1671:                                           ; preds = %L_1667.preheader, %L_1671
  %TP_0.205740 = phi i8* [ %t4060, %L_1671 ], [ getelementptr (i8, i8* @staticHeapI, i64 8696), %L_1667.preheader ]
  %t4058 = getelementptr inbounds i8, i8* %TP_0.205740, i64 8
  %t4059 = bitcast i8* %t4058 to i8**
  %t4060 = load i8*, i8** %t4059, align 8
  %t4063 = bitcast i8* %TP_0.205740 to i8**
  %t4064 = load i8*, i8** %t4063, align 8
  %t4082 = getelementptr inbounds i8, i8* %t4064, i64 8
  %t4084 = load i8, i8* %t4082, align 1
  %t4087.not.not = icmp eq i8 %t4084, 0
  %cond59.old = icmp eq i8* %t4060, inttoptr (i64 1 to i8*)
  %or.cond = select i1 %t4087.not.not, i1 true, i1 %cond59.old
  br i1 %or.cond, label %L_1669, label %L_1671

L_1669:                                           ; preds = %L_1671, %L_1667.preheader
  %t14792 = getelementptr inbounds i8, i8* %t14844, i64 -8
  %t14793 = bitcast i8* %t14792 to i8**
  store i8* %t14838, i8** %t14793, align 8
  %t14797 = bitcast i8* %t14844 to i64*
  store i64 87, i64* %t14797, align 4
  %t14799 = getelementptr inbounds i8, i8* %t14844, i64 8
  store i8* %t14841, i8** %t15197, align 8
  store i8* %t14799, i8** %t15200, align 8
  %t14806 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 256, i64 59)
  %t14809 = load i8*, i8** %t15197, align 8
  %t14812 = load i8*, i8** %t15200, align 8
  %t14785 = getelementptr inbounds i8, i8* %t14812, i64 -8
  %t14786 = bitcast i8* %t14785 to i8**
  store i8* %t14806, i8** %t14786, align 8
  %t14788 = getelementptr inbounds i8, i8* %t14812, i64 -16
  %t14789 = bitcast i8* %t14788 to i8**
  %t14790 = load i8*, i8** %t14789, align 8
  br label %L_1658

L_1664:                                           ; preds = %L_1658, %L_1658, %L_1658, %L_1658, %L_1658, %L_1663
  %TW32_0.12 = phi i32 [ %t4111, %L_1663 ], [ 1, %L_1658 ], [ 1, %L_1658 ], [ 1, %L_1658 ], [ 1, %L_1658 ], [ 1, %L_1658 ]
  %t4099 = load i8*, i8** %t14786, align 8
  %t4101 = shl nsw i64 %TW64_0.373600, 2
  %t4102 = getelementptr inbounds i8, i8* %t4099, i64 %t4101
  %t4104 = bitcast i8* %t4102 to i32*
  store i32 %TW32_0.12, i32* %t4104, align 4
  %t4107 = add nuw nsw i64 %TW64_0.373600, 1
  %t14778 = icmp ugt i64 %TW64_0.373600, 254
  br i1 %t14778, label %L_1688, label %L_1658

L_1663:                                           ; preds = %L_1658
  %t4110 = icmp eq i8 %t4129, 12
  %t4111 = zext i1 %t4110 to i32
  br label %L_1664

L_1658:                                           ; preds = %L_1669, %L_1664
  %TW64_0.373600 = phi i64 [ 0, %L_1669 ], [ %t4107, %L_1664 ]
  %t4129 = trunc i64 %TW64_0.373600 to i8
  switch i8 %t4129, label %L_1663 [
    i8 32, label %L_1664
    i8 13, label %L_1664
    i8 11, label %L_1664
    i8 10, label %L_1664
    i8 9, label %L_1664
  ]

L_825:                                            ; preds = %L_1688
  %t4147 = bitcast i8* %t14776 to i64*
  store i64 136, i64* %t4147, align 4
  %t4149 = getelementptr inbounds i8, i8* %t14776, i64 8
  store i8* %t14773, i8** %t15197, align 8
  store i8* %t4149, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4159 = load i8*, i8** %t15197, align 8
  %t4162 = load i8*, i8** %t15200, align 8
  %t4144 = getelementptr inbounds i8, i8* %t4162, i64 -96
  br label %L_827

L_830:                                            ; preds = %L_827
  %t4175 = getelementptr inbounds i8, i8* %t14727, i64 -8
  %t4176 = bitcast i8* %t4175 to i64*
  store i64 135, i64* %t4176, align 4
  store i8* %t14724, i8** %t15197, align 8
  store i8* %t14727, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4188 = load i8*, i8** %t15197, align 8
  %t4191 = load i8*, i8** %t15200, align 8
  br label %L_832

L_837:                                            ; preds = %L_832
  %t4213 = getelementptr inbounds i8, i8* %t14584, i64 -8
  %t4214 = bitcast i8* %t4213 to i64*
  store i64 90, i64* %t4214, align 4
  store i8* %t14581, i8** %t15197, align 8
  store i8* %t14584, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4226 = load i8*, i8** %t15197, align 8
  %t4229 = load i8*, i8** %t15200, align 8
  br label %L_839

L_842:                                            ; preds = %L_839
  %t4242 = getelementptr inbounds i8, i8* %t14528, i64 -8
  %t4243 = bitcast i8* %t4242 to i64*
  store i64 134, i64* %t4243, align 4
  store i8* %t14525, i8** %t15197, align 8
  store i8* %t14528, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4255 = load i8*, i8** %t15197, align 8
  %t4258 = load i8*, i8** %t15200, align 8
  %t14297.phi.trans.insert = getelementptr inbounds i8, i8* %t4258, i64 -56
  %t14298.phi.trans.insert = bitcast i8* %t14297.phi.trans.insert to i8**
  %t14299.pre = load i8*, i8** %t14298.phi.trans.insert, align 8
  br label %L_844

L_849:                                            ; preds = %L_1697
  %t4263 = bitcast i8* %stackTop.0 to i64*
  store i64 129, i64* %t4263, align 4
  %t4265 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %t4265, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4275 = load i8*, i8** %t15197, align 8
  %t4278 = load i8*, i8** %t15200, align 8
  %t4260 = getelementptr inbounds i8, i8* %t4278, i64 -144
  br label %L_851

L_853:                                            ; preds = %L_851
  %t4285 = getelementptr inbounds i8, i8* %stackTop.137, i64 136
  %t4286 = bitcast i8* %t4285 to i64*
  store i64 129, i64* %t4286, align 4
  %t4288 = getelementptr inbounds i8, i8* %stackTop.137, i64 144
  store i8* %t14151, i8** %t15197, align 8
  store i8* %t4288, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4298 = load i8*, i8** %t15197, align 8
  %t4301 = load i8*, i8** %t15200, align 8
  %t4283 = getelementptr inbounds i8, i8* %t4301, i64 -144
  br label %L_855

L_1633:                                           ; preds = %L_1631
  %t4312 = getelementptr inbounds i8, i8* %stackTop.136, i64 32
  %t4313 = bitcast i8* %t4312 to i64*
  store i64 128, i64* %t4313, align 4
  %t4315 = getelementptr inbounds i8, i8* %stackTop.136, i64 40
  store i8* %t13961, i8** %t15197, align 8
  store i8* %t4315, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4325 = load i8*, i8** %t15197, align 8
  %t4328 = load i8*, i8** %t15200, align 8
  %t4310 = getelementptr inbounds i8, i8* %t4328, i64 -40
  br label %L_1635

L_1647:                                           ; preds = %L_1647.preheader5822, %L_1647
  %TW64_1.123693 = phi i64 [ %t4353, %L_1647 ], [ %TW64_1.123693.ph, %L_1647.preheader5822 ]
  %48 = shl i64 %TW64_1.123693, 32
  %t4337 = ashr exact i64 %48, 32
  %t4341 = getelementptr inbounds i8, i8* %t4432.pre, i64 %t4337
  %t4344 = load i8, i8* %t4341, align 1
  %t4348 = getelementptr inbounds i8, i8* %t4451, i64 %TW64_1.123693
  store i8 %t4344, i8* %t4348, align 1
  %t4353 = add nuw nsw i64 %TW64_1.123693, 1
  %t4421.not = icmp slt i64 %t4353, %t4428.pre
  br i1 %t4421.not, label %L_1647, label %L_1802, !llvm.loop !15

L_1643:                                           ; preds = %L_1802
  %t4359 = bitcast i8* %t44304521 to i64*
  store i64 127, i64* %t4359, align 4
  %t4361 = getelementptr inbounds i8, i8* %stackTop.484518, i64 48
  store i8* %frontier.514519, i8** %t15197, align 8
  store i8* %t4361, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4371 = load i8*, i8** %t15197, align 8
  %t4374 = load i8*, i8** %t15200, align 8
  %t4356 = getelementptr inbounds i8, i8* %t4374, i64 -48
  %t4398.phi.trans.insert = getelementptr inbounds i8, i8* %t4374, i64 -16
  %t4399.phi.trans.insert = bitcast i8* %t4398.phi.trans.insert to i8**
  %t4400.pre = load i8*, i8** %t4399.phi.trans.insert, align 8
  br label %L_1645

L_1646:                                           ; preds = %L_1635, %L_1645
  %TP_0.21 = phi i8* [ %t4406, %L_1645 ], [ getelementptr (i8, i8* @staticHeapI, i64 3272), %L_1635 ]
  %stackTop.46 = phi i8* [ %stackTop.47, %L_1645 ], [ %stackTop.49, %L_1635 ]
  %frontier.49 = phi i8* [ %frontier.50, %L_1645 ], [ %frontier.52, %L_1635 ]
  %t4376 = getelementptr inbounds i8, i8* %frontier.49, i64 8
  %t4381 = bitcast i8* %frontier.49 to i64*
  store i64 125, i64* %t4381, align 4
  %t4386 = bitcast i8* %t4376 to i32*
  %t4388 = getelementptr inbounds i8, i8* %stackTop.46, i64 24
  %t4389 = bitcast i8* %t4388 to i32*
  %t4390 = load i32, i32* %t4389, align 4
  store i32 %t4390, i32* %t4386, align 4
  %t4392 = getelementptr inbounds i8, i8* %frontier.49, i64 16
  %t4393 = bitcast i8* %t4392 to i8**
  store i8* %TP_0.21, i8** %t4393, align 8
  br label %L_786

L_1645:                                           ; preds = %L_1802, %L_1643
  %t4399.pre-phi = phi i8** [ %t4410, %L_1802 ], [ %t4399.phi.trans.insert, %L_1643 ]
  %t4400 = phi i8* [ %TP_0.224517, %L_1802 ], [ %t4400.pre, %L_1643 ]
  %stackTop.47 = phi i8* [ %stackTop.484518, %L_1802 ], [ %t4356, %L_1643 ]
  %frontier.50 = phi i8* [ %frontier.514519, %L_1802 ], [ %t4371, %L_1643 ]
  %t4401 = getelementptr inbounds i8, i8* %t4400, i64 -8
  %t4402 = bitcast i8* %t4401 to i64*
  store i64 11, i64* %t4402, align 4
  %t4406 = load i8*, i8** %t4399.pre-phi, align 8
  br label %L_1646

L_1802:                                           ; preds = %L_1647, %middle.block5768, %L_join_12.thread, %L_join_12
  %t44304521 = phi i8* [ %t44304515, %L_join_12.thread ], [ %t4430, %L_join_12 ], [ %t4430, %middle.block5768 ], [ %t4430, %L_1647 ]
  %t44264520 = phi i8* [ %t44264514, %L_join_12.thread ], [ %t4426, %L_join_12 ], [ %t4426, %middle.block5768 ], [ %t4426, %L_1647 ]
  %frontier.514519 = phi i8* [ %frontier.52, %L_join_12.thread ], [ %t4454, %L_join_12 ], [ %t4454, %middle.block5768 ], [ %t4454, %L_1647 ]
  %stackTop.484518 = phi i8* [ %stackTop.49, %L_join_12.thread ], [ %t4434, %L_join_12 ], [ %t4434, %middle.block5768 ], [ %t4434, %L_1647 ]
  %TP_0.224517 = phi i8* [ getelementptr (i8, i8* @staticHeapM, i64 64), %L_join_12.thread ], [ %t4451, %L_join_12 ], [ %t4451, %middle.block5768 ], [ %t4451, %L_1647 ]
  %t4410 = bitcast i8* %t44264520 to i8**
  store i8* %TP_0.224517, i8** %t4410, align 8
  %t4414 = load i8*, i8** %t14203, align 8
  %t4416.not = icmp ult i8* %t4414, %frontier.514519
  br i1 %t4416.not, label %L_1643, label %L_1645

L_join_12:                                        ; preds = %L_1640
  %t4441 = getelementptr inbounds i8, i8* %stackTop.49, i64 48
  %t4442 = bitcast i8* %t4441 to i64*
  store i64 126, i64* %t4442, align 4
  %t4444 = getelementptr inbounds i8, i8* %stackTop.49, i64 56
  store i8* %frontier.52, i8** %t15197, align 8
  store i8* %t4444, i8** %t15200, align 8
  %t4451 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t4484, i64 21)
  %t4454 = load i8*, i8** %t15197, align 8
  %t4457 = load i8*, i8** %t15200, align 8
  %t4434 = getelementptr inbounds i8, i8* %t4457, i64 -56
  %t4426.phi.trans.insert = getelementptr inbounds i8, i8* %t4457, i64 -24
  %t4427.phi.trans.insert = bitcast i8* %t4426.phi.trans.insert to i64*
  %t4428.pre = load i64, i64* %t4427.phi.trans.insert, align 4
  %t4430.phi.trans.insert = getelementptr inbounds i8, i8* %t4457, i64 -16
  %t4431.phi.trans.insert = bitcast i8* %t4430.phi.trans.insert to i8**
  %t4432.pre = load i8*, i8** %t4431.phi.trans.insert, align 8
  %t4426 = getelementptr inbounds i8, i8* %t4457, i64 -24
  %t4430 = getelementptr inbounds i8, i8* %t4457, i64 -16
  %t4421.not3692 = icmp sgt i64 %t4428.pre, 0
  br i1 %t4421.not3692, label %L_1647.preheader, label %L_1802

L_1647.preheader:                                 ; preds = %L_join_12
  %49 = add i64 %t4428.pre, -4
  %50 = icmp ult i64 %49, 2147483645
  br i1 %50, label %vector.memcheck5762, label %L_1647.preheader5822

vector.memcheck5762:                              ; preds = %L_1647.preheader
  %scevgep5763 = getelementptr i8, i8* %t4451, i64 %t4428.pre
  %scevgep5764 = getelementptr i8, i8* %t4432.pre, i64 %t4428.pre
  %bound05765 = icmp ult i8* %t4451, %scevgep5764
  %bound15766 = icmp ult i8* %t4432.pre, %scevgep5763
  %found.conflict5767 = and i1 %bound05765, %bound15766
  br i1 %found.conflict5767, label %L_1647.preheader5822, label %vector.ph5771

vector.ph5771:                                    ; preds = %vector.memcheck5762
  %n.vec = and i64 %t4428.pre, -4
  br label %vector.body5770

vector.body5770:                                  ; preds = %vector.body5770, %vector.ph5771
  %index5774 = phi i64 [ 0, %vector.ph5771 ], [ %index.next5776, %vector.body5770 ]
  %51 = shl i64 %index5774, 32
  %52 = ashr exact i64 %51, 32
  %53 = getelementptr inbounds i8, i8* %t4432.pre, i64 %52
  %54 = bitcast i8* %53 to <4 x i8>*
  %wide.load5775 = load <4 x i8>, <4 x i8>* %54, align 1, !alias.scope !16
  %55 = getelementptr inbounds i8, i8* %t4451, i64 %index5774
  %56 = bitcast i8* %55 to <4 x i8>*
  store <4 x i8> %wide.load5775, <4 x i8>* %56, align 1, !alias.scope !19, !noalias !16
  %index.next5776 = add nuw i64 %index5774, 4
  %57 = icmp eq i64 %index.next5776, %n.vec
  br i1 %57, label %middle.block5768, label %vector.body5770, !llvm.loop !21

middle.block5768:                                 ; preds = %vector.body5770
  %cmp.n5773 = icmp eq i64 %t4428.pre, %n.vec
  br i1 %cmp.n5773, label %L_1802, label %L_1647.preheader5822

L_1647.preheader5822:                             ; preds = %vector.memcheck5762, %L_1647.preheader, %middle.block5768
  %TW64_1.123693.ph = phi i64 [ 0, %vector.memcheck5762 ], [ 0, %L_1647.preheader ], [ %n.vec, %middle.block5768 ]
  br label %L_1647

L_1640:                                           ; preds = %L_1801
  %t4462.not = icmp eq i32 %TW32_0.13, 0
  br i1 %t4462.not, label %L_join_12.thread, label %L_join_12

L_join_12.thread:                                 ; preds = %L_1640
  %t44264514 = getelementptr inbounds i8, i8* %stackTop.49, i64 32
  %t44304515 = getelementptr inbounds i8, i8* %stackTop.49, i64 40
  br label %L_1802

L_1801:                                           ; preds = %loop_86
  %t4466 = getelementptr inbounds i8, i8* %stackTop.49, i64 32
  %t4467 = bitcast i8* %t4466 to i64*
  store i64 %t4484, i64* %t4467, align 4
  %t4470 = getelementptr inbounds i8, i8* %stackTop.49, i64 40
  %t4471 = bitcast i8* %t4470 to i8**
  store i8* %t4496, i8** %t4471, align 8
  %trunc1781 = icmp sgt i32 %TW32_0.13, -1
  br i1 %trunc1781, label %L_1640, label %L_786

loop_86:                                          ; preds = %L_1635, %loop_86
  %TW32_0.13 = phi i32 [ %t4481, %loop_86 ], [ 0, %L_1635 ]
  %t4484 = sext i32 %TW32_0.13 to i64
  %t4488 = getelementptr inbounds i8, i8* %t4496, i64 %t4484
  %t4491 = load i8, i8* %t4488, align 1
  %cond = icmp eq i8 %t4491, 0
  %t4481 = add i32 %TW32_0.13, 1
  br i1 %cond, label %L_1801, label %loop_86

L_1635:                                           ; preds = %L_1631, %L_1633
  %stackTop.49 = phi i8* [ %t4310, %L_1633 ], [ %stackTop.136, %L_1631 ]
  %frontier.52 = phi i8* [ %t4325, %L_1633 ], [ %t13961, %L_1631 ]
  %t4502 = getelementptr inbounds i8, i8* %stackTop.49, i64 24
  %t4503 = bitcast i8* %t4502 to i32*
  %t4504 = load i32, i32* %t4503, align 4
  %t4505 = tail call i64 @Posix_Error_strError(i32 %t4504)
  %t4496 = inttoptr i64 %t4505 to i8*
  %t4498.not = icmp eq i64 %t4505, 0
  br i1 %t4498.not, label %L_1646, label %loop_86

L_1631:                                           ; preds = %L_1629
  %t4509 = add i32 %t4526, -1
  store i32 %t4509, i32* %t13969, align 4
  %t4515 = load i8*, i8** %t14203, align 8
  %t4517.not = icmp ult i8* %t4515, %t13961
  br i1 %t4517.not, label %L_1633, label %L_1635

L_1629:                                           ; preds = %L_855
  %t4531 = tail call i32 @Posix_Error_getErrno()
  %t4522 = getelementptr inbounds i8, i8* %stackTop.136, i64 24
  %t4523 = bitcast i8* %t4522 to i32*
  store i32 %t4531, i32* %t4523, align 4
  %t4526 = load i32, i32* %t13969, align 4
  %t4528.not = icmp eq i32 %t4526, 0
  br i1 %t4528.not, label %L_786, label %L_1631

L_883:                                            ; preds = %L_881
  %t4540 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t4541 = bitcast i8* %t4540 to i64*
  store i64 133, i64* %t4541, align 4
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %stackTop.0, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4553 = load i8*, i8** %t15197, align 8
  %t4556 = load i8*, i8** %t15200, align 8
  br label %L_885

L_1626:                                           ; preds = %L_885
  %stackTop.135 = getelementptr inbounds i8, i8* %t4556.pn, i64 -168
  %t4558 = getelementptr inbounds i8, i8* %frontier.133, i64 8
  %t4563 = bitcast i8* %frontier.133 to i64*
  store i64 41, i64* %t4563, align 4
  %t4565 = getelementptr inbounds i8, i8* %frontier.133, i64 16
  %t4568 = bitcast i8* %t4558 to i64*
  store i64 0, i64* %t4568, align 4
  br label %L_892

L_889:                                            ; preds = %L_1702
  store i64 132, i64* %t13754, align 4
  %t4575 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %t4575, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4585 = load i8*, i8** %t15197, align 8
  %t4588 = load i8*, i8** %t15200, align 8
  %t4570 = getelementptr inbounds i8, i8* %t4588, i64 -184
  br label %L_891

L_1622:                                           ; preds = %L_1619
  %t4601 = getelementptr inbounds i8, i8* %t4687, i64 -8
  %t4602 = bitcast i8* %t4601 to i64*
  store i64 92, i64* %t4602, align 4
  store i8* %t4684, i8** %t15197, align 8
  store i8* %t4687, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4614 = load i8*, i8** %t15197, align 8
  %t4617 = load i8*, i8** %t15200, align 8
  br label %L_1624

L_1624:                                           ; preds = %L_1619, %L_1622
  %t4617.pn = phi i8* [ %t4617, %L_1622 ], [ %t4687, %L_1619 ]
  %frontier.53 = phi i8* [ %t4614, %L_1622 ], [ %t4684, %L_1619 ]
  %t4619 = getelementptr inbounds i8, i8* %frontier.53, i64 8
  %t4624 = bitcast i8* %frontier.53 to i64*
  store i64 121, i64* %t4624, align 4
  %t4629 = bitcast i8* %t4619 to i8**
  %t4631 = getelementptr inbounds i8, i8* %t4617.pn, i64 -152
  %t4632 = bitcast i8* %t4631 to i8**
  %t4633 = load i8*, i8** %t4632, align 8
  store i8* %t4633, i8** %t4629, align 8
  %t4635 = getelementptr inbounds i8, i8* %frontier.53, i64 16
  %t4636 = bitcast i8* %t4635 to i8**
  store i8* getelementptr (i8, i8* @staticHeapM, i64 8), i8** %t4636, align 8
  br label %L_899

L_1619:                                           ; preds = %L_892
  store i64 131, i64* %t13649, align 4
  %t4674 = getelementptr inbounds i8, i8* %stackTop.133, i64 184
  store i8* %t13715, i8** %t15197, align 8
  store i8* %t4674, i8** %t15200, align 8
  %t4681 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 4096, i64 21)
  %t4684 = load i8*, i8** %t15197, align 8
  %t4687 = load i8*, i8** %t15200, align 8
  call void @llvm.memset.p0i8.i64(i8* noundef nonnull align 1 dereferenceable(4096) %t4681, i8 0, i64 4096, i1 false)
  %t4642 = getelementptr inbounds i8, i8* %t4687, i64 -152
  %t4643 = bitcast i8* %t4642 to i8**
  store i8* %t4681, i8** %t4643, align 8
  %t4649 = load i8*, i8** %t14203, align 8
  %t4651.not = icmp ult i8* %t4649, %t4684
  br i1 %t4651.not, label %L_1622, label %L_1624

L_896:                                            ; preds = %L_893
  %t4700 = getelementptr inbounds i8, i8* %t13664, i64 -8
  %t4701 = bitcast i8* %t4700 to i64*
  store i64 130, i64* %t4701, align 4
  store i8* %t13661, i8** %t15197, align 8
  store i8* %t13664, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4713 = load i8*, i8** %t15197, align 8
  %t4716 = load i8*, i8** %t15200, align 8
  br label %L_898

L_904:                                            ; preds = %L_1704
  %t4724 = bitcast i8* %t13516 to i64*
  store i64 129, i64* %t4724, align 4
  %t4726 = getelementptr inbounds i8, i8* %t4617.pn.pn, i64 -40
  store i8* %t13575, i8** %t15197, align 8
  store i8* %t4726, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4736 = load i8*, i8** %t15197, align 8
  %t4739 = load i8*, i8** %t15200, align 8
  %t4721 = getelementptr inbounds i8, i8* %t4739, i64 -144
  br label %L_906

L_1599:                                           ; preds = %L_1597
  %t4746 = getelementptr inbounds i8, i8* %stackTop.130, i64 32
  %t4747 = bitcast i8* %t4746 to i64*
  store i64 128, i64* %t4747, align 4
  %t4749 = getelementptr inbounds i8, i8* %stackTop.130, i64 40
  store i8* %t13425, i8** %t15197, align 8
  store i8* %t4749, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4759 = load i8*, i8** %t15197, align 8
  %t4762 = load i8*, i8** %t15200, align 8
  %t4744 = getelementptr inbounds i8, i8* %t4762, i64 -40
  br label %L_1601

L_1613:                                           ; preds = %L_1613.preheader5821, %L_1613
  %TW64_1.133695 = phi i64 [ %t4787, %L_1613 ], [ %TW64_1.133695.ph, %L_1613.preheader5821 ]
  %58 = shl i64 %TW64_1.133695, 32
  %t4771 = ashr exact i64 %58, 32
  %t4775 = getelementptr inbounds i8, i8* %t4866.pre, i64 %t4771
  %t4778 = load i8, i8* %t4775, align 1
  %t4782 = getelementptr inbounds i8, i8* %t4885, i64 %TW64_1.133695
  store i8 %t4778, i8* %t4782, align 1
  %t4787 = add nuw nsw i64 %TW64_1.133695, 1
  %t4855.not = icmp slt i64 %t4787, %t4862.pre
  br i1 %t4855.not, label %L_1613, label %L_1799, !llvm.loop !22

L_1609:                                           ; preds = %L_1799
  %t4793 = bitcast i8* %t48644534 to i64*
  store i64 127, i64* %t4793, align 4
  %t4795 = getelementptr inbounds i8, i8* %stackTop.534531, i64 48
  store i8* %frontier.564532, i8** %t15197, align 8
  store i8* %t4795, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4805 = load i8*, i8** %t15197, align 8
  %t4808 = load i8*, i8** %t15200, align 8
  %t4790 = getelementptr inbounds i8, i8* %t4808, i64 -48
  %t4832.phi.trans.insert = getelementptr inbounds i8, i8* %t4808, i64 -16
  %t4833.phi.trans.insert = bitcast i8* %t4832.phi.trans.insert to i8**
  %t4834.pre = load i8*, i8** %t4833.phi.trans.insert, align 8
  br label %L_1611

L_1612:                                           ; preds = %L_1601, %L_1611
  %TP_0.23 = phi i8* [ %t4840, %L_1611 ], [ getelementptr (i8, i8* @staticHeapI, i64 3272), %L_1601 ]
  %stackTop.51 = phi i8* [ %stackTop.52, %L_1611 ], [ %stackTop.54, %L_1601 ]
  %frontier.54 = phi i8* [ %frontier.55, %L_1611 ], [ %frontier.57, %L_1601 ]
  %t4810 = getelementptr inbounds i8, i8* %frontier.54, i64 8
  %t4815 = bitcast i8* %frontier.54 to i64*
  store i64 125, i64* %t4815, align 4
  %t4820 = bitcast i8* %t4810 to i32*
  %t4822 = getelementptr inbounds i8, i8* %stackTop.51, i64 24
  %t4823 = bitcast i8* %t4822 to i32*
  %t4824 = load i32, i32* %t4823, align 4
  store i32 %t4824, i32* %t4820, align 4
  %t4826 = getelementptr inbounds i8, i8* %frontier.54, i64 16
  %t4827 = bitcast i8* %t4826 to i8**
  store i8* %TP_0.23, i8** %t4827, align 8
  br label %L_786

L_1611:                                           ; preds = %L_1799, %L_1609
  %t4833.pre-phi = phi i8** [ %t4844, %L_1799 ], [ %t4833.phi.trans.insert, %L_1609 ]
  %t4834 = phi i8* [ %TP_0.244530, %L_1799 ], [ %t4834.pre, %L_1609 ]
  %stackTop.52 = phi i8* [ %stackTop.534531, %L_1799 ], [ %t4790, %L_1609 ]
  %frontier.55 = phi i8* [ %frontier.564532, %L_1799 ], [ %t4805, %L_1609 ]
  %t4835 = getelementptr inbounds i8, i8* %t4834, i64 -8
  %t4836 = bitcast i8* %t4835 to i64*
  store i64 11, i64* %t4836, align 4
  %t4840 = load i8*, i8** %t4833.pre-phi, align 8
  br label %L_1612

L_1799:                                           ; preds = %L_1613, %middle.block5784, %L_join_11.thread, %L_join_11
  %t48644534 = phi i8* [ %t48644528, %L_join_11.thread ], [ %t4864, %L_join_11 ], [ %t4864, %middle.block5784 ], [ %t4864, %L_1613 ]
  %t48604533 = phi i8* [ %t48604527, %L_join_11.thread ], [ %t4860, %L_join_11 ], [ %t4860, %middle.block5784 ], [ %t4860, %L_1613 ]
  %frontier.564532 = phi i8* [ %frontier.57, %L_join_11.thread ], [ %t4888, %L_join_11 ], [ %t4888, %middle.block5784 ], [ %t4888, %L_1613 ]
  %stackTop.534531 = phi i8* [ %stackTop.54, %L_join_11.thread ], [ %t4868, %L_join_11 ], [ %t4868, %middle.block5784 ], [ %t4868, %L_1613 ]
  %TP_0.244530 = phi i8* [ getelementptr (i8, i8* @staticHeapM, i64 64), %L_join_11.thread ], [ %t4885, %L_join_11 ], [ %t4885, %middle.block5784 ], [ %t4885, %L_1613 ]
  %t4844 = bitcast i8* %t48604533 to i8**
  store i8* %TP_0.244530, i8** %t4844, align 8
  %t4848 = load i8*, i8** %t14203, align 8
  %t4850.not = icmp ult i8* %t4848, %frontier.564532
  br i1 %t4850.not, label %L_1609, label %L_1611

L_join_11:                                        ; preds = %L_1606
  %t4875 = getelementptr inbounds i8, i8* %stackTop.54, i64 48
  %t4876 = bitcast i8* %t4875 to i64*
  store i64 126, i64* %t4876, align 4
  %t4878 = getelementptr inbounds i8, i8* %stackTop.54, i64 56
  store i8* %frontier.57, i8** %t15197, align 8
  store i8* %t4878, i8** %t15200, align 8
  %t4885 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t4918, i64 21)
  %t4888 = load i8*, i8** %t15197, align 8
  %t4891 = load i8*, i8** %t15200, align 8
  %t4868 = getelementptr inbounds i8, i8* %t4891, i64 -56
  %t4860.phi.trans.insert = getelementptr inbounds i8, i8* %t4891, i64 -24
  %t4861.phi.trans.insert = bitcast i8* %t4860.phi.trans.insert to i64*
  %t4862.pre = load i64, i64* %t4861.phi.trans.insert, align 4
  %t4864.phi.trans.insert = getelementptr inbounds i8, i8* %t4891, i64 -16
  %t4865.phi.trans.insert = bitcast i8* %t4864.phi.trans.insert to i8**
  %t4866.pre = load i8*, i8** %t4865.phi.trans.insert, align 8
  %t4860 = getelementptr inbounds i8, i8* %t4891, i64 -24
  %t4864 = getelementptr inbounds i8, i8* %t4891, i64 -16
  %t4855.not3694 = icmp sgt i64 %t4862.pre, 0
  br i1 %t4855.not3694, label %L_1613.preheader, label %L_1799

L_1613.preheader:                                 ; preds = %L_join_11
  %59 = add i64 %t4862.pre, -4
  %60 = icmp ult i64 %59, 2147483645
  br i1 %60, label %vector.memcheck5778, label %L_1613.preheader5821

vector.memcheck5778:                              ; preds = %L_1613.preheader
  %scevgep5779 = getelementptr i8, i8* %t4885, i64 %t4862.pre
  %scevgep5780 = getelementptr i8, i8* %t4866.pre, i64 %t4862.pre
  %bound05781 = icmp ult i8* %t4885, %scevgep5780
  %bound15782 = icmp ult i8* %t4866.pre, %scevgep5779
  %found.conflict5783 = and i1 %bound05781, %bound15782
  br i1 %found.conflict5783, label %L_1613.preheader5821, label %vector.ph5788

vector.ph5788:                                    ; preds = %vector.memcheck5778
  %n.vec5790 = and i64 %t4862.pre, -4
  br label %vector.body5786

vector.body5786:                                  ; preds = %vector.body5786, %vector.ph5788
  %index5793 = phi i64 [ 0, %vector.ph5788 ], [ %index.next5795, %vector.body5786 ]
  %61 = shl i64 %index5793, 32
  %62 = ashr exact i64 %61, 32
  %63 = getelementptr inbounds i8, i8* %t4866.pre, i64 %62
  %64 = bitcast i8* %63 to <4 x i8>*
  %wide.load5794 = load <4 x i8>, <4 x i8>* %64, align 1, !alias.scope !23
  %65 = getelementptr inbounds i8, i8* %t4885, i64 %index5793
  %66 = bitcast i8* %65 to <4 x i8>*
  store <4 x i8> %wide.load5794, <4 x i8>* %66, align 1, !alias.scope !26, !noalias !23
  %index.next5795 = add nuw i64 %index5793, 4
  %67 = icmp eq i64 %index.next5795, %n.vec5790
  br i1 %67, label %middle.block5784, label %vector.body5786, !llvm.loop !28

middle.block5784:                                 ; preds = %vector.body5786
  %cmp.n5792 = icmp eq i64 %t4862.pre, %n.vec5790
  br i1 %cmp.n5792, label %L_1799, label %L_1613.preheader5821

L_1613.preheader5821:                             ; preds = %vector.memcheck5778, %L_1613.preheader, %middle.block5784
  %TW64_1.133695.ph = phi i64 [ 0, %vector.memcheck5778 ], [ 0, %L_1613.preheader ], [ %n.vec5790, %middle.block5784 ]
  br label %L_1613

L_1606:                                           ; preds = %L_1798
  %t4896.not = icmp eq i32 %TW32_0.14, 0
  br i1 %t4896.not, label %L_join_11.thread, label %L_join_11

L_join_11.thread:                                 ; preds = %L_1606
  %t48604527 = getelementptr inbounds i8, i8* %stackTop.54, i64 32
  %t48644528 = getelementptr inbounds i8, i8* %stackTop.54, i64 40
  br label %L_1799

L_1798:                                           ; preds = %loop_83
  %t4900 = getelementptr inbounds i8, i8* %stackTop.54, i64 32
  %t4901 = bitcast i8* %t4900 to i64*
  store i64 %t4918, i64* %t4901, align 4
  %t4904 = getelementptr inbounds i8, i8* %stackTop.54, i64 40
  %t4905 = bitcast i8* %t4904 to i8**
  store i8* %t4930, i8** %t4905, align 8
  %trunc1825 = icmp sgt i32 %TW32_0.14, -1
  br i1 %trunc1825, label %L_1606, label %L_786

loop_83:                                          ; preds = %L_1601, %loop_83
  %TW32_0.14 = phi i32 [ %t4915, %loop_83 ], [ 0, %L_1601 ]
  %t4918 = sext i32 %TW32_0.14 to i64
  %t4922 = getelementptr inbounds i8, i8* %t4930, i64 %t4918
  %t4925 = load i8, i8* %t4922, align 1
  %cond2 = icmp eq i8 %t4925, 0
  %t4915 = add i32 %TW32_0.14, 1
  br i1 %cond2, label %L_1798, label %loop_83

L_1601:                                           ; preds = %L_1597, %L_1599
  %stackTop.54 = phi i8* [ %t4744, %L_1599 ], [ %stackTop.130, %L_1597 ]
  %frontier.57 = phi i8* [ %t4759, %L_1599 ], [ %t13425, %L_1597 ]
  %t4936 = getelementptr inbounds i8, i8* %stackTop.54, i64 24
  %t4937 = bitcast i8* %t4936 to i32*
  %t4938 = load i32, i32* %t4937, align 4
  %t4939 = tail call i64 @Posix_Error_strError(i32 %t4938)
  %t4930 = inttoptr i64 %t4939 to i8*
  %t4932.not = icmp eq i64 %t4939, 0
  br i1 %t4932.not, label %L_1612, label %loop_83

L_1597:                                           ; preds = %L_1595
  %t4943 = add i32 %t4960, -1
  store i32 %t4943, i32* %t13969, align 4
  %t4949 = load i8*, i8** %t14203, align 8
  %t4951.not = icmp ult i8* %t4949, %t13425
  br i1 %t4951.not, label %L_1599, label %L_1601

L_1595:                                           ; preds = %L_906
  %t4965 = tail call i32 @Posix_Error_getErrno()
  %t4956 = getelementptr inbounds i8, i8* %stackTop.130, i64 24
  %t4957 = bitcast i8* %t4956 to i32*
  store i32 %t4965, i32* %t4957, align 4
  %t4960 = load i32, i32* %t13969, align 4
  %t4962.not = icmp eq i32 %t4960, 0
  br i1 %t4962.not, label %L_786, label %L_1597

L_1592.sink.split:                                ; preds = %L_761, %L_754, %L_773, %L_762
  %.sink5214 = phi i8* [ inttoptr (i64 1 to i8*), %L_762 ], [ inttoptr (i64 1 to i8*), %L_773 ], [ inttoptr (i64 7 to i8*), %L_754 ], [ inttoptr (i64 7 to i8*), %L_761 ]
  %t3759 = load i8*, i8** %t14423, align 8
  %t3762 = load i64, i64* %t15180, align 4
  %t3763 = getelementptr inbounds i8, i8* %t3759, i64 %t3762
  %t3766 = bitcast i8* %t3763 to i8**
  store i8* %.sink5214, i8** %t3766, align 8
  %t3770 = load i8*, i8** %t14423, align 8
  %t3773 = load i64, i64* %t15180, align 4
  %t3774 = getelementptr inbounds i8, i8* %t3770, i64 %t3773
  br label %L_1592

L_1592:                                           ; preds = %doSwitchNextBlock, %L_1592.sink.split
  %stackTop.55 = phi i8* [ %t3774, %L_1592.sink.split ], [ %stackTop.0, %doSwitchNextBlock ]
  %t4975 = bitcast i8* %stackTop.55 to i8**
  %t4976 = load i8*, i8** %t4975, align 8
  br label %L_786

L_1574:                                           ; preds = %L_1572
  %t4983 = getelementptr inbounds i8, i8* %stackTop.129, i64 32
  %t4984 = bitcast i8* %t4983 to i64*
  store i64 128, i64* %t4984, align 4
  %t4986 = getelementptr inbounds i8, i8* %stackTop.129, i64 40
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %t4986, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t4996 = load i8*, i8** %t15197, align 8
  %t4999 = load i8*, i8** %t15200, align 8
  %t4981 = getelementptr inbounds i8, i8* %t4999, i64 -40
  br label %L_1576

L_1588:                                           ; preds = %L_1588.preheader5820, %L_1588
  %TW64_1.143697 = phi i64 [ %t5024, %L_1588 ], [ %TW64_1.143697.ph, %L_1588.preheader5820 ]
  %68 = shl i64 %TW64_1.143697, 32
  %t5008 = ashr exact i64 %68, 32
  %t5012 = getelementptr inbounds i8, i8* %t5103.pre, i64 %t5008
  %t5015 = load i8, i8* %t5012, align 1
  %t5019 = getelementptr inbounds i8, i8* %t5122, i64 %TW64_1.143697
  store i8 %t5015, i8* %t5019, align 1
  %t5024 = add nuw nsw i64 %TW64_1.143697, 1
  %t5092.not = icmp slt i64 %t5024, %t5099.pre
  br i1 %t5092.not, label %L_1588, label %L_1797, !llvm.loop !29

L_1584:                                           ; preds = %L_1797
  %t5030 = bitcast i8* %t51014547 to i64*
  store i64 127, i64* %t5030, align 4
  %t5032 = getelementptr inbounds i8, i8* %stackTop.584544, i64 48
  store i8* %frontier.604545, i8** %t15197, align 8
  store i8* %t5032, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t5042 = load i8*, i8** %t15197, align 8
  %t5045 = load i8*, i8** %t15200, align 8
  %t5027 = getelementptr inbounds i8, i8* %t5045, i64 -48
  %t5069.phi.trans.insert = getelementptr inbounds i8, i8* %t5045, i64 -16
  %t5070.phi.trans.insert = bitcast i8* %t5069.phi.trans.insert to i8**
  %t5071.pre = load i8*, i8** %t5070.phi.trans.insert, align 8
  br label %L_1586

L_1587:                                           ; preds = %L_1576, %L_1586
  %TP_0.25 = phi i8* [ %t5077, %L_1586 ], [ getelementptr (i8, i8* @staticHeapI, i64 3272), %L_1576 ]
  %stackTop.56 = phi i8* [ %stackTop.57, %L_1586 ], [ %stackTop.59, %L_1576 ]
  %frontier.58 = phi i8* [ %frontier.59, %L_1586 ], [ %frontier.61, %L_1576 ]
  %t5047 = getelementptr inbounds i8, i8* %frontier.58, i64 8
  %t5052 = bitcast i8* %frontier.58 to i64*
  store i64 125, i64* %t5052, align 4
  %t5057 = bitcast i8* %t5047 to i32*
  %t5059 = getelementptr inbounds i8, i8* %stackTop.56, i64 24
  %t5060 = bitcast i8* %t5059 to i32*
  %t5061 = load i32, i32* %t5060, align 4
  store i32 %t5061, i32* %t5057, align 4
  %t5063 = getelementptr inbounds i8, i8* %frontier.58, i64 16
  %t5064 = bitcast i8* %t5063 to i8**
  store i8* %TP_0.25, i8** %t5064, align 8
  br label %L_786

L_1586:                                           ; preds = %L_1797, %L_1584
  %t5070.pre-phi = phi i8** [ %t5081, %L_1797 ], [ %t5070.phi.trans.insert, %L_1584 ]
  %t5071 = phi i8* [ %TP_0.264543, %L_1797 ], [ %t5071.pre, %L_1584 ]
  %stackTop.57 = phi i8* [ %stackTop.584544, %L_1797 ], [ %t5027, %L_1584 ]
  %frontier.59 = phi i8* [ %frontier.604545, %L_1797 ], [ %t5042, %L_1584 ]
  %t5072 = getelementptr inbounds i8, i8* %t5071, i64 -8
  %t5073 = bitcast i8* %t5072 to i64*
  store i64 11, i64* %t5073, align 4
  %t5077 = load i8*, i8** %t5070.pre-phi, align 8
  br label %L_1587

L_1797:                                           ; preds = %L_1588, %middle.block5803, %L_join_10.thread, %L_join_10
  %t51014547 = phi i8* [ %t51014541, %L_join_10.thread ], [ %t5101, %L_join_10 ], [ %t5101, %middle.block5803 ], [ %t5101, %L_1588 ]
  %t50974546 = phi i8* [ %t50974540, %L_join_10.thread ], [ %t5097, %L_join_10 ], [ %t5097, %middle.block5803 ], [ %t5097, %L_1588 ]
  %frontier.604545 = phi i8* [ %frontier.61, %L_join_10.thread ], [ %t5125, %L_join_10 ], [ %t5125, %middle.block5803 ], [ %t5125, %L_1588 ]
  %stackTop.584544 = phi i8* [ %stackTop.59, %L_join_10.thread ], [ %t5105, %L_join_10 ], [ %t5105, %middle.block5803 ], [ %t5105, %L_1588 ]
  %TP_0.264543 = phi i8* [ getelementptr (i8, i8* @staticHeapM, i64 64), %L_join_10.thread ], [ %t5122, %L_join_10 ], [ %t5122, %middle.block5803 ], [ %t5122, %L_1588 ]
  %t5081 = bitcast i8* %t50974546 to i8**
  store i8* %TP_0.264543, i8** %t5081, align 8
  %t5085 = load i8*, i8** %t14203, align 8
  %t5087.not = icmp ult i8* %t5085, %frontier.604545
  br i1 %t5087.not, label %L_1584, label %L_1586

L_join_10:                                        ; preds = %L_1581
  %t5112 = getelementptr inbounds i8, i8* %stackTop.59, i64 48
  %t5113 = bitcast i8* %t5112 to i64*
  store i64 126, i64* %t5113, align 4
  %t5115 = getelementptr inbounds i8, i8* %stackTop.59, i64 56
  store i8* %frontier.61, i8** %t15197, align 8
  store i8* %t5115, i8** %t15200, align 8
  %t5122 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t5155, i64 21)
  %t5125 = load i8*, i8** %t15197, align 8
  %t5128 = load i8*, i8** %t15200, align 8
  %t5105 = getelementptr inbounds i8, i8* %t5128, i64 -56
  %t5097.phi.trans.insert = getelementptr inbounds i8, i8* %t5128, i64 -24
  %t5098.phi.trans.insert = bitcast i8* %t5097.phi.trans.insert to i64*
  %t5099.pre = load i64, i64* %t5098.phi.trans.insert, align 4
  %t5101.phi.trans.insert = getelementptr inbounds i8, i8* %t5128, i64 -16
  %t5102.phi.trans.insert = bitcast i8* %t5101.phi.trans.insert to i8**
  %t5103.pre = load i8*, i8** %t5102.phi.trans.insert, align 8
  %t5097 = getelementptr inbounds i8, i8* %t5128, i64 -24
  %t5101 = getelementptr inbounds i8, i8* %t5128, i64 -16
  %t5092.not3696 = icmp sgt i64 %t5099.pre, 0
  br i1 %t5092.not3696, label %L_1588.preheader, label %L_1797

L_1588.preheader:                                 ; preds = %L_join_10
  %69 = add i64 %t5099.pre, -4
  %70 = icmp ult i64 %69, 2147483645
  br i1 %70, label %vector.memcheck5797, label %L_1588.preheader5820

vector.memcheck5797:                              ; preds = %L_1588.preheader
  %scevgep5798 = getelementptr i8, i8* %t5122, i64 %t5099.pre
  %scevgep5799 = getelementptr i8, i8* %t5103.pre, i64 %t5099.pre
  %bound05800 = icmp ult i8* %t5122, %scevgep5799
  %bound15801 = icmp ult i8* %t5103.pre, %scevgep5798
  %found.conflict5802 = and i1 %bound05800, %bound15801
  br i1 %found.conflict5802, label %L_1588.preheader5820, label %vector.ph5807

vector.ph5807:                                    ; preds = %vector.memcheck5797
  %n.vec5809 = and i64 %t5099.pre, -4
  br label %vector.body5805

vector.body5805:                                  ; preds = %vector.body5805, %vector.ph5807
  %index5812 = phi i64 [ 0, %vector.ph5807 ], [ %index.next5814, %vector.body5805 ]
  %71 = shl i64 %index5812, 32
  %72 = ashr exact i64 %71, 32
  %73 = getelementptr inbounds i8, i8* %t5103.pre, i64 %72
  %74 = bitcast i8* %73 to <4 x i8>*
  %wide.load5813 = load <4 x i8>, <4 x i8>* %74, align 1, !alias.scope !30
  %75 = getelementptr inbounds i8, i8* %t5122, i64 %index5812
  %76 = bitcast i8* %75 to <4 x i8>*
  store <4 x i8> %wide.load5813, <4 x i8>* %76, align 1, !alias.scope !33, !noalias !30
  %index.next5814 = add nuw i64 %index5812, 4
  %77 = icmp eq i64 %index.next5814, %n.vec5809
  br i1 %77, label %middle.block5803, label %vector.body5805, !llvm.loop !35

middle.block5803:                                 ; preds = %vector.body5805
  %cmp.n5811 = icmp eq i64 %t5099.pre, %n.vec5809
  br i1 %cmp.n5811, label %L_1797, label %L_1588.preheader5820

L_1588.preheader5820:                             ; preds = %vector.memcheck5797, %L_1588.preheader, %middle.block5803
  %TW64_1.143697.ph = phi i64 [ 0, %vector.memcheck5797 ], [ 0, %L_1588.preheader ], [ %n.vec5809, %middle.block5803 ]
  br label %L_1588

L_1581:                                           ; preds = %L_1796
  %t5133.not = icmp eq i32 %TW32_0.15, 0
  br i1 %t5133.not, label %L_join_10.thread, label %L_join_10

L_join_10.thread:                                 ; preds = %L_1581
  %t50974540 = getelementptr inbounds i8, i8* %stackTop.59, i64 32
  %t51014541 = getelementptr inbounds i8, i8* %stackTop.59, i64 40
  br label %L_1797

L_1796:                                           ; preds = %loop_81
  %t5137 = getelementptr inbounds i8, i8* %stackTop.59, i64 32
  %t5138 = bitcast i8* %t5137 to i64*
  store i64 %t5155, i64* %t5138, align 4
  %t5141 = getelementptr inbounds i8, i8* %stackTop.59, i64 40
  %t5142 = bitcast i8* %t5141 to i8**
  store i8* %t5167, i8** %t5142, align 8
  %trunc1843 = icmp sgt i32 %TW32_0.15, -1
  br i1 %trunc1843, label %L_1581, label %L_786

loop_81:                                          ; preds = %L_1576, %loop_81
  %TW32_0.15 = phi i32 [ %t5152, %loop_81 ], [ 0, %L_1576 ]
  %t5155 = sext i32 %TW32_0.15 to i64
  %t5159 = getelementptr inbounds i8, i8* %t5167, i64 %t5155
  %t5162 = load i8, i8* %t5159, align 1
  %cond3 = icmp eq i8 %t5162, 0
  %t5152 = add i32 %TW32_0.15, 1
  br i1 %cond3, label %L_1796, label %loop_81

L_1576:                                           ; preds = %L_1572, %L_1574
  %stackTop.59 = phi i8* [ %t4981, %L_1574 ], [ %stackTop.129, %L_1572 ]
  %frontier.61 = phi i8* [ %t4996, %L_1574 ], [ %frontier.0, %L_1572 ]
  %t5173 = getelementptr inbounds i8, i8* %stackTop.59, i64 24
  %t5174 = bitcast i8* %t5173 to i32*
  %t5175 = load i32, i32* %t5174, align 4
  %t5176 = tail call i64 @Posix_Error_strError(i32 %t5175)
  %t5167 = inttoptr i64 %t5176 to i8*
  %t5169.not = icmp eq i64 %t5176, 0
  br i1 %t5169.not, label %L_1587, label %loop_81

L_1572:                                           ; preds = %L_1570
  %t5180 = add i32 %t5197, -1
  store i32 %t5180, i32* %t13969, align 4
  %t5186 = load i8*, i8** %t14203, align 8
  %t5188.not = icmp ult i8* %t5186, %frontier.0
  br i1 %t5188.not, label %L_1574, label %L_1576

L_1570:                                           ; preds = %isReg_0
  %t5202 = tail call i32 @Posix_Error_getErrno()
  %t5193 = getelementptr inbounds i8, i8* %stackTop.129, i64 24
  %t5194 = bitcast i8* %t5193 to i32*
  store i32 %t5202, i32* %t5194, align 4
  %t5197 = load i32, i32* %t13969, align 4
  %t5199.not = icmp eq i32 %t5197, 0
  br i1 %t5199.not, label %L_786, label %L_1572

L_961:                                            ; preds = %L_956
  %t5221 = bitcast i8* %t13029 to i64*
  store i64 125, i64* %t5221, align 4
  %t5223 = getelementptr inbounds i8, i8* %t13029, i64 8
  store i8* %t13026, i8** %t15197, align 8
  store i8* %t5223, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t5233 = load i8*, i8** %t15197, align 8
  %t5236 = load i8*, i8** %t15200, align 8
  %t5218 = getelementptr inbounds i8, i8* %t5236, i64 -168
  br label %L_963

L_1566:                                           ; preds = %L_1714
  %t12929.le5203 = bitcast i8* %t12928 to i8**
  %t5238 = getelementptr inbounds i8, i8* %stackTop.128, i64 24
  %t5239 = bitcast i8* %t5238 to i8**
  %t5243 = load i8*, i8** %t12929.le5203, align 8
  store i8* %t5243, i8** %t5239, align 8
  %t5245 = getelementptr inbounds i8, i8* %stackTop.128, i64 56
  br label %L_981

L_1565:                                           ; preds = %L_1563
  %t12929.le5201 = bitcast i8* %t12928 to i8**
  %t5250 = getelementptr inbounds i8, i8* %stackTop.128, i64 24
  %t5251 = bitcast i8* %t5250 to i8**
  %t5255 = load i8*, i8** %t12929.le5201, align 8
  store i8* %t5255, i8** %t5251, align 8
  %t5257 = getelementptr inbounds i8, i8* %stackTop.128, i64 56
  br label %L_981

L_1564:                                           ; preds = %L_1563
  %t5265 = bitcast i8* %TP_0.673457 to i8**
  %t5266 = load i8*, i8** %t5265, align 8
  %t5268 = add nuw nsw i64 %TW64_1.253458, 1
  %t12802.not = icmp slt i64 %t5268, %t1281246354639
  br i1 %t12802.not, label %L_1563, label %L_969.preheader

L_1563:                                           ; preds = %L_1563.preheader, %L_1564
  %TW64_1.253458 = phi i64 [ %t5268, %L_1564 ], [ 0, %L_1563.preheader ]
  %TP_0.673457 = phi i8* [ %t5266, %L_1564 ], [ getelementptr (i8, i8* @staticHeapI, i64 9168), %L_1563.preheader ]
  %cond5 = icmp eq i8* %TP_0.673457, inttoptr (i64 1 to i8*)
  br i1 %cond5, label %L_1565, label %L_1564

L_1562:                                           ; preds = %L_1716
  %t12929.le5199 = bitcast i8* %t12928 to i8**
  %t5274 = getelementptr inbounds i8, i8* %stackTop.128, i64 24
  %t5275 = bitcast i8* %t5274 to i8**
  %t5279 = load i8*, i8** %t12929.le5199, align 8
  store i8* %t5279, i8** %t5275, align 8
  %t5281 = getelementptr inbounds i8, i8* %stackTop.128, i64 56
  br label %L_981

L_1561:                                           ; preds = %L_1559
  %t12929.le = bitcast i8* %t12928 to i8**
  %t5286 = getelementptr inbounds i8, i8* %stackTop.128, i64 24
  %t5287 = bitcast i8* %t5286 to i8**
  %t5291 = load i8*, i8** %t12929.le, align 8
  store i8* %t5291, i8** %t5287, align 8
  %t5293 = getelementptr inbounds i8, i8* %stackTop.128, i64 56
  br label %L_981

L_1560:                                           ; preds = %L_1559
  %t5301 = bitcast i8* %TP_0.653463 to i8**
  %t5302 = load i8*, i8** %t5301, align 8
  %t5304 = add nuw nsw i64 %TW64_1.243464, 1
  %t12769.not = icmp slt i64 %t5304, %t1277946264630
  br i1 %t12769.not, label %L_1559, label %L_974.preheader

L_1559:                                           ; preds = %L_1559.preheader, %L_1560
  %TW64_1.243464 = phi i64 [ %t5304, %L_1560 ], [ 0, %L_1559.preheader ]
  %TP_0.653463 = phi i8* [ %t5302, %L_1560 ], [ getelementptr (i8, i8* @staticHeapI, i64 9168), %L_1559.preheader ]
  %cond7 = icmp eq i8* %TP_0.653463, inttoptr (i64 1 to i8*)
  br i1 %cond7, label %L_1561, label %L_1560

L_1557:                                           ; preds = %L_1002
  %t5310 = getelementptr inbounds i8, i8* %stackTop.125, i64 24
  %t5311 = bitcast i8* %t5310 to i8**
  %t5313 = getelementptr inbounds i8, i8* %stackTop.125, i64 160
  %t5314 = bitcast i8* %t5313 to i8**
  %t5315 = load i8*, i8** %t5314, align 8
  store i8* %t5315, i8** %t5311, align 8
  %t5317 = getelementptr inbounds i8, i8* %stackTop.125, i64 56
  br label %L_981

L_1556:                                           ; preds = %L_1554
  %t12406.le = getelementptr inbounds i8, i8* %t12445, i64 -176
  %t5322 = getelementptr inbounds i8, i8* %t12445, i64 -152
  %t5323 = bitcast i8* %t5322 to i8**
  %t5325 = getelementptr inbounds i8, i8* %t12445, i64 -16
  %t5326 = bitcast i8* %t5325 to i8**
  %t5327 = load i8*, i8** %t5326, align 8
  store i8* %t5327, i8** %t5323, align 8
  %t5329 = getelementptr inbounds i8, i8* %t12445, i64 -120
  br label %L_981

L_1555:                                           ; preds = %L_1554
  %t5336 = getelementptr inbounds i8, i8* %TP_3.03478, i64 8
  %t5337 = bitcast i8* %t5336 to i8**
  %t5338 = load i8*, i8** %t5337, align 8
  %t5341 = bitcast i8* %TP_3.03478 to i32*
  %t5342 = load i32, i32* %t5341, align 4
  %t5345 = shl nsw i64 %TW64_1.233479, 2
  %t5346 = getelementptr inbounds i8, i8* %t12439, i64 %t5345
  %t5348 = bitcast i8* %t5346 to i32*
  store i32 %t5342, i32* %t5348, align 4
  %t5351 = add nuw nsw i64 %TW64_1.233479, 1
  %t12402.not = icmp slt i64 %t5351, %t12411
  br i1 %t12402.not, label %L_1554, label %loop_44.preheader

L_1554:                                           ; preds = %L_1554.preheader, %L_1555
  %TW64_1.233479 = phi i64 [ %t5351, %L_1555 ], [ 0, %L_1554.preheader ]
  %TP_3.03478 = phi i8* [ %t5338, %L_1555 ], [ %t12423, %L_1554.preheader ]
  %cond12 = icmp eq i8* %TP_3.03478, inttoptr (i64 1 to i8*)
  br i1 %cond12, label %L_1556, label %L_1555

L_1551:                                           ; preds = %loop_44.preheader, %L_1551
  %TW64_0.263480 = phi i64 [ %t5378, %L_1551 ], [ 0, %loop_44.preheader ]
  %78 = shl i64 %TW64_0.263480, 32
  %t5395 = ashr exact i64 %78, 32
  %t5366 = getelementptr inbounds i8, i8* getelementptr (i8, i8* @staticHeapI, i64 3312), i64 %t5395
  %t5369 = load i8, i8* %t5366, align 1
  %t5373 = getelementptr inbounds i8, i8* %t12368, i64 %TW64_0.263480
  store i8 %t5369, i8* %t5373, align 1
  %t5378 = add nuw nsw i64 %TW64_0.263480, 1
  %t12335 = icmp ugt i64 %TW64_0.263480, 62
  br i1 %t12335, label %L_1015.preheader.preheader, label %L_1551, !llvm.loop !36

L_1545:                                           ; preds = %L_1544
  %t11602.le = getelementptr inbounds i8, i8* %t11637, i64 -176
  %t5425 = getelementptr inbounds i8, i8* %t11637, i64 -152
  %t5426 = bitcast i8* %t5425 to i8**
  %t5428 = getelementptr inbounds i8, i8* %t11637, i64 -16
  %t5429 = bitcast i8* %t5428 to i8**
  %t5430 = load i8*, i8** %t5429, align 8
  store i8* %t5430, i8** %t5426, align 8
  %t5432 = getelementptr inbounds i8, i8* %t11637, i64 -120
  br label %L_981

L_1544:                                           ; preds = %L_1090, %loop_53
  %TW64_0.233494 = phi i64 [ %t5422, %loop_53 ], [ 0, %L_1090 ]
  %79 = and i64 %TW64_0.233494, 4294967040
  %t5441.not = icmp eq i64 %79, 0
  br i1 %t5441.not, label %loop_53, label %L_1545

L_1541:                                           ; preds = %L_1540
  %t11487.le = getelementptr inbounds i8, i8* %t11522, i64 -176
  %t5457 = getelementptr inbounds i8, i8* %t11522, i64 -152
  %t5458 = bitcast i8* %t5457 to i8**
  %t5460 = getelementptr inbounds i8, i8* %t11522, i64 -16
  %t5461 = bitcast i8* %t5460 to i8**
  %t5462 = load i8*, i8** %t5461, align 8
  store i8* %t5462, i8** %t5458, align 8
  %t5464 = getelementptr inbounds i8, i8* %t11522, i64 -120
  br label %L_981

L_1540:                                           ; preds = %L_1097, %loop_56
  %TW64_0.203497 = phi i64 [ %t5454, %loop_56 ], [ 0, %L_1097 ]
  %80 = and i64 %TW64_0.203497, 4294967040
  %t5473.not = icmp eq i64 %80, 0
  br i1 %t5473.not, label %loop_56, label %L_1541

L_1539:                                           ; preds = %L_1735
  %t11354 = getelementptr inbounds i8, i8* %stackTop.0, i64 -176
  %t5477 = getelementptr inbounds i8, i8* %stackTop.0, i64 -152
  %t5478 = bitcast i8* %t5477 to i8**
  %t5480 = getelementptr inbounds i8, i8* %stackTop.0, i64 -16
  %t5481 = bitcast i8* %t5480 to i8**
  %t5482 = load i8*, i8** %t5481, align 8
  store i8* %t5482, i8** %t5478, align 8
  %t5484 = getelementptr inbounds i8, i8* %stackTop.0, i64 -120
  br label %L_981

L_1538:                                           ; preds = %L_1533
  %t5489 = getelementptr inbounds i8, i8* %stackTop.1243509, i64 24
  %t5490 = bitcast i8* %t5489 to i8**
  %t5492 = getelementptr inbounds i8, i8* %stackTop.1243509, i64 160
  %t5493 = bitcast i8* %t5492 to i8**
  %t5494 = load i8*, i8** %t5493, align 8
  store i8* %t5494, i8** %t5490, align 8
  %t5496 = getelementptr inbounds i8, i8* %stackTop.1243509, i64 56
  br label %L_981

L_1537:                                           ; preds = %L_1537.lr.ph, %L_1537
  %TW64_0.123500 = phi i64 [ 0, %L_1537.lr.ph ], [ %t5525, %L_1537 ]
  %81 = shl i64 %TW64_0.123500, 32
  %t5506 = ashr exact i64 %81, 32
  %t5510 = load i8*, i8** %t5509, align 8
  %t5513 = getelementptr inbounds i8, i8* %t5510, i64 %t5506
  %t5516 = load i8, i8* %t5513, align 1
  %t5520 = getelementptr inbounds i8, i8* %t5619, i64 %TW64_0.123500
  store i8 %t5516, i8* %t5520, align 1
  %t5525 = add nuw nsw i64 %TW64_0.123500, 1
  %t5567 = load i64, i64* %t5566.phi.trans.insert, align 4
  %t5568.not = icmp slt i64 %t5525, %t5567
  br i1 %t5568.not, label %L_1537, label %L_1536

L_1536:                                           ; preds = %L_1537, %L_1534, %L_join_9
  %frontier.624558 = phi i8* [ %t5622, %L_join_9 ], [ %frontier.1233510, %L_1534 ], [ %t5622, %L_1537 ]
  %stackTop.604557 = phi i8* [ %t5572, %L_join_9 ], [ %stackTop.1243509, %L_1534 ], [ %t5572, %L_1537 ]
  %TP_0.274556 = phi i8* [ %t5619, %L_join_9 ], [ getelementptr (i8, i8* @staticHeapM, i64 64), %L_1534 ], [ %t5619, %L_1537 ]
  %TP_1.44555 = phi i8* [ %t5577, %L_join_9 ], [ %TP_0.623508, %L_1534 ], [ %t5577, %L_1537 ]
  %t5528 = getelementptr inbounds i8, i8* %TP_0.274556, i64 -8
  %t5529 = bitcast i8* %t5528 to i64*
  store i64 11, i64* %t5529, align 4
  %t5531 = ptrtoint i8* %TP_1.44555 to i64
  %t5533 = lshr i64 %t5531, 8
  %t5536 = load i8*, i8** %t14171, align 8
  %t5539 = getelementptr inbounds i8, i8* %t5536, i64 %t5533
  store i8 1, i8* %t5539, align 1
  %t5544 = getelementptr inbounds i8, i8* %stackTop.604557, i64 64
  %t5545 = bitcast i8* %t5544 to i64*
  %t5546 = load i64, i64* %t5545, align 4
  %t5547 = shl nsw i64 %t5546, 3
  %t5548 = getelementptr inbounds i8, i8* %TP_1.44555, i64 %t5547
  %t5550 = bitcast i8* %t5548 to i8**
  store i8* %TP_0.274556, i8** %t5550, align 8
  %t5556 = load i64, i64* %t5545, align 4
  %t5557 = add i64 %t5556, 1
  store i64 %t5557, i64* %t5545, align 4
  %t11303 = getelementptr inbounds i8, i8* %stackTop.604557, i64 96
  %t11304 = bitcast i8* %t11303 to i64*
  %t11305 = load i64, i64* %t11304, align 4
  %t11306.not = icmp slt i64 %t5557, %t11305
  br i1 %t11306.not, label %L_1531, label %L_1736.loopexit

L_join_9:                                         ; preds = %L_1534
  %t5587 = getelementptr inbounds i8, i8* %stackTop.1243509, i64 168
  %t5588 = bitcast i8* %t5587 to i8**
  store i8* %TP_0.623508, i8** %t5588, align 8
  %t5591 = getelementptr inbounds i8, i8* %stackTop.1243509, i64 176
  %t5592 = bitcast i8* %t5591 to i8**
  %t5594 = getelementptr inbounds i8, i8* %stackTop.1243509, i64 136
  %t5595 = bitcast i8* %t5594 to i8**
  %t5596 = load i8*, i8** %t5595, align 8
  store i8* %t5596, i8** %t5592, align 8
  %t5598 = getelementptr inbounds i8, i8* %stackTop.1243509, i64 184
  %t5599 = bitcast i8* %t5598 to i8**
  %t5601 = getelementptr inbounds i8, i8* %stackTop.1243509, i64 144
  %t5602 = bitcast i8* %t5601 to i8**
  %t5603 = load i8*, i8** %t5602, align 8
  store i8* %t5603, i8** %t5599, align 8
  %t5609 = getelementptr inbounds i8, i8* %stackTop.1243509, i64 192
  %t5610 = bitcast i8* %t5609 to i64*
  store i64 124, i64* %t5610, align 4
  %t5612 = getelementptr inbounds i8, i8* %stackTop.1243509, i64 200
  store i8* %frontier.1233510, i8** %t15197, align 8
  store i8* %t5612, i8** %t15200, align 8
  %t5619 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t5649, i64 21)
  %t5622 = load i8*, i8** %t15197, align 8
  %t5625 = load i8*, i8** %t15200, align 8
  %t5572 = getelementptr inbounds i8, i8* %t5625, i64 -200
  %t5575 = getelementptr inbounds i8, i8* %t5625, i64 -32
  %t5576 = bitcast i8* %t5575 to i8**
  %t5577 = load i8*, i8** %t5576, align 8
  %t5565.phi.trans.insert = getelementptr inbounds i8, i8* %t5625, i64 -48
  %t5566.phi.trans.insert = bitcast i8* %t5565.phi.trans.insert to i64*
  %t55673498.pre = load i64, i64* %t5566.phi.trans.insert, align 4
  %t5568.not3499 = icmp sgt i64 %t55673498.pre, 0
  br i1 %t5568.not3499, label %L_1537.lr.ph, label %L_1536

L_1537.lr.ph:                                     ; preds = %L_join_9
  %t5508 = getelementptr inbounds i8, i8* %t5625, i64 -120
  %t5509 = bitcast i8* %t5508 to i8**
  br label %L_1537

L_1534:                                           ; preds = %L_1533
  %t5630.not = icmp eq i32 %TW32_0.16, 0
  br i1 %t5630.not, label %L_1536, label %L_join_9

L_1533:                                           ; preds = %loop_79
  %trunc1955 = icmp sgt i32 %TW32_0.16, -1
  br i1 %trunc1955, label %L_1534, label %L_1538

loop_79:                                          ; preds = %loop_79, %L_1531
  %TW32_0.16 = phi i32 [ 0, %L_1531 ], [ %t5641, %loop_79 ]
  store i8* %t5685, i8** %t5647, align 8
  %t5649 = sext i32 %TW32_0.16 to i64
  store i64 %t5649, i64* %t5652, align 4
  %t5662 = getelementptr inbounds i8, i8* %t5685, i64 %t5649
  %t5665 = load i8, i8* %t5662, align 1
  %cond13 = icmp eq i8 %t5665, 0
  %t5641 = add i32 %TW32_0.16, 1
  br i1 %cond13, label %L_1533, label %loop_79

L_1531:                                           ; preds = %L_1103, %L_1536
  %t113013511 = phi i64 [ %t5557, %L_1536 ], [ 0, %L_1103 ]
  %frontier.1233510 = phi i8* [ %frontier.624558, %L_1536 ], [ %t11349, %L_1103 ]
  %stackTop.1243509 = phi i8* [ %stackTop.604557, %L_1536 ], [ %t11310, %L_1103 ]
  %TP_0.623508 = phi i8* [ %TP_1.44555, %L_1536 ], [ %t11346, %L_1103 ]
  %t5673 = getelementptr inbounds i8, i8* %stackTop.1243509, i64 88
  %t5674 = bitcast i8* %t5673 to i64*
  %t5675 = load i64, i64* %t5674, align 4
  %t5676 = inttoptr i64 %t5675 to i8*
  %82 = shl i64 %t113013511, 32
  %t5681 = ashr exact i64 %82, 29
  %t5682 = getelementptr inbounds i8, i8* %t5676, i64 %t5681
  %t5684 = bitcast i8* %t5682 to i8**
  %t5685 = load i8*, i8** %t5684, align 8
  %t5646 = getelementptr inbounds i8, i8* %stackTop.1243509, i64 80
  %t5647 = bitcast i8* %t5646 to i8**
  %t5651 = getelementptr inbounds i8, i8* %stackTop.1243509, i64 152
  %t5652 = bitcast i8* %t5651 to i64*
  br label %loop_79

L_1794:                                           ; preds = %loop_58
  %t5714 = getelementptr inbounds i8, i8* %stackTop.123, i64 88
  %t5715 = bitcast i8* %t5714 to i64*
  store i64 %TW64_0.19, i64* %t5715, align 4
  %t5718 = getelementptr inbounds i8, i8* %stackTop.123, i64 96
  %t5719 = bitcast i8* %t5718 to i8**
  store i8* %TP_0.61, i8** %t5719, align 8
  %t5722 = getelementptr inbounds i8, i8* %stackTop.123, i64 136
  %t5723 = bitcast i8* %t5722 to i8**
  store i8* %TP_1.20, i8** %t5723, align 8
  %t5726 = getelementptr inbounds i8, i8* %stackTop.123, i64 144
  %t5727 = bitcast i8* %t5726 to i8**
  %t5729 = getelementptr inbounds i8, i8* %stackTop.123, i64 64
  %t5730 = bitcast i8* %t5729 to i8**
  %t5731 = load i8*, i8** %t5730, align 8
  store i8* %t5731, i8** %t5727, align 8
  %t5733 = getelementptr inbounds i8, i8* %stackTop.123, i64 152
  %t5734 = bitcast i8* %t5733 to i8**
  %t5736 = getelementptr inbounds i8, i8* %stackTop.123, i64 80
  %t5737 = bitcast i8* %t5736 to i8**
  %t5738 = load i8*, i8** %t5737, align 8
  store i8* %t5738, i8** %t5734, align 8
  %t5740 = getelementptr inbounds i8, i8* %stackTop.123, i64 168
  %t5741 = bitcast i8* %t5740 to i64*
  store i64 123, i64* %t5741, align 4
  %t5743 = getelementptr inbounds i8, i8* %stackTop.123, i64 176
  store i8* %frontier.122, i8** %t15197, align 8
  store i8* %t5743, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t5753 = load i8*, i8** %t15197, align 8
  %t5756 = load i8*, i8** %t15200, align 8
  %t5689 = getelementptr inbounds i8, i8* %t5756, i64 -176
  %t5691 = getelementptr inbounds i8, i8* %t5756, i64 -88
  %t5692 = bitcast i8* %t5691 to i64*
  %t5693 = load i64, i64* %t5692, align 4
  %t5695 = getelementptr inbounds i8, i8* %t5756, i64 -80
  %t5696 = bitcast i8* %t5695 to i8**
  %t5697 = load i8*, i8** %t5696, align 8
  %t5699 = getelementptr inbounds i8, i8* %t5756, i64 -40
  %t5700 = bitcast i8* %t5699 to i8**
  %t5701 = load i8*, i8** %t5700, align 8
  br label %L_1108

L_1529:                                           ; preds = %L_1738, %L_1115
  %t5758 = getelementptr inbounds i8, i8* %stackTop.122, i64 24
  %t5759 = bitcast i8* %t5758 to i8**
  %t5761 = getelementptr inbounds i8, i8* %stackTop.122, i64 160
  %t5762 = bitcast i8* %t5761 to i8**
  %t5763 = load i8*, i8** %t5762, align 8
  store i8* %t5763, i8** %t5759, align 8
  %t5765 = getelementptr inbounds i8, i8* %stackTop.122, i64 56
  br label %L_981

L_1793:                                           ; preds = %loop_59
  %t5797 = getelementptr inbounds i8, i8* %stackTop.121, i64 88
  %t5798 = bitcast i8* %t5797 to i32*
  store i32 %TW32_0.33, i32* %t5798, align 4
  %t5801 = getelementptr inbounds i8, i8* %stackTop.121, i64 96
  %t5802 = bitcast i8* %t5801 to i8**
  store i8* %TP_0.57, i8** %t5802, align 8
  %t5805 = getelementptr inbounds i8, i8* %stackTop.121, i64 136
  %t5806 = bitcast i8* %t5805 to i8**
  store i8* %TP_1.18, i8** %t5806, align 8
  %t5809 = getelementptr inbounds i8, i8* %stackTop.121, i64 144
  %t5810 = bitcast i8* %t5809 to i8**
  %t5812 = getelementptr inbounds i8, i8* %stackTop.121, i64 64
  %t5813 = bitcast i8* %t5812 to i8**
  %t5814 = load i8*, i8** %t5813, align 8
  store i8* %t5814, i8** %t5810, align 8
  %t5816 = getelementptr inbounds i8, i8* %stackTop.121, i64 152
  %t5817 = bitcast i8* %t5816 to i8**
  %t5819 = getelementptr inbounds i8, i8* %stackTop.121, i64 80
  %t5820 = bitcast i8* %t5819 to i8**
  %t5821 = load i8*, i8** %t5820, align 8
  store i8* %t5821, i8** %t5817, align 8
  %t5823 = getelementptr inbounds i8, i8* %stackTop.121, i64 168
  %t5824 = bitcast i8* %t5823 to i64*
  store i64 123, i64* %t5824, align 4
  %t5826 = getelementptr inbounds i8, i8* %stackTop.121, i64 176
  store i8* %frontier.120, i8** %t15197, align 8
  store i8* %t5826, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t5836 = load i8*, i8** %t15197, align 8
  %t5839 = load i8*, i8** %t15200, align 8
  %t5772 = getelementptr inbounds i8, i8* %t5839, i64 -176
  %t5774 = getelementptr inbounds i8, i8* %t5839, i64 -88
  %t5775 = bitcast i8* %t5774 to i32*
  %t5776 = load i32, i32* %t5775, align 4
  %t5778 = getelementptr inbounds i8, i8* %t5839, i64 -80
  %t5779 = bitcast i8* %t5778 to i8**
  %t5780 = load i8*, i8** %t5779, align 8
  %t5782 = getelementptr inbounds i8, i8* %t5839, i64 -40
  %t5783 = bitcast i8* %t5782 to i8**
  %t5784 = load i8*, i8** %t5783, align 8
  br label %L_1121

L_1528:                                           ; preds = %L_1504
  %t6230 = add nsw i32 %TW32_0.32, 1
  %t5841 = getelementptr inbounds i8, i8* %frontier.119, i64 8
  %t5846 = bitcast i8* %frontier.119 to i64*
  store i64 91, i64* %t5846, align 4
  %t5848 = getelementptr inbounds i8, i8* %frontier.119, i64 16
  %t5851 = bitcast i8* %t5841 to i8**
  store i8* %TP_1.17, i8** %t5851, align 8
  br label %loop_59

L_1792:                                           ; preds = %L_1509
  %t5891 = getelementptr inbounds i8, i8* %stackTop.63, i64 184
  %t5892 = bitcast i8* %t5891 to i8**
  store i8* %TP_1.7, i8** %t5892, align 8
  %t5895 = getelementptr inbounds i8, i8* %stackTop.63, i64 192
  %t5896 = bitcast i8* %t5895 to i8**
  store i8* %TP_0.31, i8** %t5896, align 8
  %t5899 = getelementptr inbounds i8, i8* %stackTop.63, i64 200
  %t5900 = bitcast i8* %t5899 to i32*
  %t5902 = getelementptr inbounds i8, i8* %stackTop.63, i64 80
  %t5903 = bitcast i8* %t5902 to i32*
  %t5904 = load i32, i32* %t5903, align 4
  store i32 %t5904, i32* %t5900, align 4
  %t5906 = getelementptr inbounds i8, i8* %stackTop.63, i64 204
  %t5907 = bitcast i8* %t5906 to i32*
  %t5909 = getelementptr inbounds i8, i8* %stackTop.63, i64 84
  %t5910 = bitcast i8* %t5909 to i32*
  %t5911 = load i32, i32* %t5910, align 4
  store i32 %t5911, i32* %t5907, align 4
  %t5913 = getelementptr inbounds i8, i8* %stackTop.63, i64 208
  %t5914 = bitcast i8* %t5913 to i8**
  %t5916 = getelementptr inbounds i8, i8* %stackTop.63, i64 152
  %t5917 = bitcast i8* %t5916 to i8**
  %t5918 = load i8*, i8** %t5917, align 8
  store i8* %t5918, i8** %t5914, align 8
  %t5920 = getelementptr inbounds i8, i8* %stackTop.63, i64 216
  %t5921 = bitcast i8* %t5920 to i8**
  %t5923 = getelementptr inbounds i8, i8* %stackTop.63, i64 168
  %t5924 = bitcast i8* %t5923 to i8**
  %t5925 = load i8*, i8** %t5924, align 8
  store i8* %t5925, i8** %t5921, align 8
  %t5927 = getelementptr inbounds i8, i8* %stackTop.63, i64 224
  %t5928 = bitcast i8* %t5927 to i8**
  %t5930 = getelementptr inbounds i8, i8* %stackTop.63, i64 176
  %t5931 = bitcast i8* %t5930 to i8**
  %t5932 = load i8*, i8** %t5931, align 8
  store i8* %t5932, i8** %t5928, align 8
  %t5934 = getelementptr inbounds i8, i8* %stackTop.63, i64 232
  %t5935 = bitcast i8* %t5934 to i64*
  store i64 122, i64* %t5935, align 4
  %t5937 = getelementptr inbounds i8, i8* %stackTop.63, i64 240
  store i8* %frontier.65, i8** %t15197, align 8
  store i8* %t5937, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t5947 = load i8*, i8** %t15197, align 8
  %t5950 = load i8*, i8** %t15200, align 8
  %t5858 = getelementptr inbounds i8, i8* %t5950, i64 -240
  %t5860 = getelementptr inbounds i8, i8* %t5950, i64 -56
  %t5861 = bitcast i8* %t5860 to i8**
  %t5862 = load i8*, i8** %t5861, align 8
  %t5864 = getelementptr inbounds i8, i8* %t5950, i64 -48
  %t5865 = bitcast i8* %t5864 to i8**
  %t5866 = load i8*, i8** %t5865, align 8
  br label %L_1512

L_1525:                                           ; preds = %L_1520
  %t5952 = getelementptr inbounds i8, i8* %stackTop.62, i64 24
  %t5953 = bitcast i8* %t5952 to i8**
  %t5955 = getelementptr inbounds i8, i8* %stackTop.62, i64 160
  %t5956 = bitcast i8* %t5955 to i8**
  %t5957 = load i8*, i8** %t5956, align 8
  store i8* %t5957, i8** %t5953, align 8
  %t5959 = getelementptr inbounds i8, i8* %stackTop.62, i64 56
  br label %L_981

L_1524:                                           ; preds = %L_1522
  %t5964 = getelementptr inbounds i8, i8* %stackTop.612601, i64 24
  %t5965 = bitcast i8* %t5964 to i8**
  %t5967 = getelementptr inbounds i8, i8* %stackTop.612601, i64 160
  %t5968 = bitcast i8* %t5967 to i8**
  %t5969 = load i8*, i8** %t5968, align 8
  store i8* %t5969, i8** %t5965, align 8
  %t5971 = getelementptr inbounds i8, i8* %stackTop.612601, i64 56
  br label %L_981

L_1523:                                           ; preds = %L_1522
  %t5979 = bitcast i8* %TP_0.283521 to i8**
  %t5980 = load i8*, i8** %t5979, align 8
  %t5982 = add nuw nsw i64 %TW64_1.153522, 1
  %t6010.not = icmp slt i64 %t5982, %t60342594
  br i1 %t6010.not, label %L_1522, label %L_1139

L_1522:                                           ; preds = %L_1521, %L_1523
  %TW64_1.153522 = phi i64 [ %t5982, %L_1523 ], [ 0, %L_1521 ]
  %TP_0.283521 = phi i8* [ %t5980, %L_1523 ], [ %TP_0.292599, %L_1521 ]
  %cond21 = icmp eq i8* %TP_0.283521, inttoptr (i64 1 to i8*)
  br i1 %cond21, label %L_1524, label %L_1523

L_1521:                                           ; preds = %L_1787, %L_1515, %L_1520
  %frontier.632603 = phi i8* [ %t6116, %L_1520 ], [ %t6116, %L_1515 ], [ %frontier.119, %L_1787 ]
  %stackTop.612601 = phi i8* [ %stackTop.62, %L_1520 ], [ %stackTop.62, %L_1515 ], [ %stackTop.120, %L_1787 ]
  %TP_0.292599 = phi i8* [ %t6109, %L_1520 ], [ inttoptr (i64 1 to i8*), %L_1515 ], [ inttoptr (i64 1 to i8*), %L_1787 ]
  %t60342594 = phi i64 [ %t6034, %L_1520 ], [ 0, %L_1515 ], [ 0, %L_1787 ]
  %t6014 = getelementptr inbounds i8, i8* %stackTop.612601, i64 72
  %t6015 = bitcast i8* %t6014 to i32*
  %t6016 = load i32, i32* %t6015, align 4
  %t6018 = getelementptr inbounds i8, i8* %stackTop.612601, i64 76
  %t6019 = bitcast i8* %t6018 to i32*
  %t6020 = load i32, i32* %t6019, align 4
  %t6022 = getelementptr inbounds i8, i8* %stackTop.612601, i64 96
  %t6023 = bitcast i8* %t6022 to i8**
  %t6024 = load i8*, i8** %t6023, align 8
  %t6030 = getelementptr inbounds i8, i8* %stackTop.612601, i64 144
  %t6031 = bitcast i8* %t6030 to i8**
  %t6032 = load i8*, i8** %t6031, align 8
  %t6010.not3520 = icmp sgt i64 %t60342594, 0
  br i1 %t6010.not3520, label %L_1522, label %L_1139

L_1520:                                           ; preds = %L_1517.L_1517_crit_edge, %L_1517.preheader
  %t6052.lcssa = phi i32 [ 1, %L_1517.preheader ], [ %t6052, %L_1517.L_1517_crit_edge ]
  %t6034 = sext i32 %t6052.lcssa to i64
  %trunc1966 = icmp sgt i32 %t6052.lcssa, -1
  br i1 %trunc1966, label %L_1521, label %L_1525

L_1517.L_1517_crit_edge:                          ; preds = %L_1517.preheader, %L_1517.L_1517_crit_edge
  %t60525735 = phi i32 [ %t6052, %L_1517.L_1517_crit_edge ], [ 1, %L_1517.preheader ]
  %TP_1.55734 = phi i8* [ %TP_1.5.pre, %L_1517.L_1517_crit_edge ], [ %TP_1.6, %L_1517.preheader ]
  %TP_1.5.in.phi.trans.insert = bitcast i8* %TP_1.55734 to i8**
  %TP_1.5.pre = load i8*, i8** %TP_1.5.in.phi.trans.insert, align 8
  %t6052 = add i32 %t60525735, 1
  %cond20 = icmp eq i8* %TP_1.5.pre, inttoptr (i64 1 to i8*)
  br i1 %cond20, label %L_1520, label %L_1517.L_1517_crit_edge

L_1515:                                           ; preds = %L_1512
  %cond19 = icmp eq i8* %t6109, inttoptr (i64 1 to i8*)
  br i1 %cond19, label %L_1521, label %L_1517.preheader

L_1517.preheader:                                 ; preds = %L_1515
  %cond205733 = icmp eq i8* %TP_1.6, inttoptr (i64 1 to i8*)
  br i1 %cond205733, label %L_1520, label %L_1517.L_1517_crit_edge

L_1513:                                           ; preds = %L_1512
  %t6104 = bitcast i8* %TP_0.30 to i8**
  %t6105 = load i8*, i8** %t6104, align 8
  br label %L_1509

L_1512:                                           ; preds = %L_1509, %L_1792
  %TP_1.6 = phi i8* [ %t5862, %L_1792 ], [ %TP_1.7, %L_1509 ]
  %TP_0.30 = phi i8* [ %t5866, %L_1792 ], [ %TP_0.31, %L_1509 ]
  %stackTop.62 = phi i8* [ %t5858, %L_1792 ], [ %stackTop.63, %L_1509 ]
  %frontier.64 = phi i8* [ %t5947, %L_1792 ], [ %frontier.65, %L_1509 ]
  %t6109 = getelementptr inbounds i8, i8* %frontier.64, i64 8
  %t6114 = bitcast i8* %frontier.64 to i64*
  store i64 99, i64* %t6114, align 4
  %t6116 = getelementptr inbounds i8, i8* %frontier.64, i64 16
  %t6119 = bitcast i8* %t6109 to i8**
  store i8* %TP_1.6, i8** %t6119, align 8
  %cond18 = icmp eq i8* %TP_0.30, inttoptr (i64 1 to i8*)
  br i1 %cond18, label %L_1515, label %L_1513

L_1509:                                           ; preds = %L_1508, %L_1513
  %TP_1.7 = phi i8* [ inttoptr (i64 1 to i8*), %L_1508 ], [ %t6109, %L_1513 ]
  %TP_0.31 = phi i8* [ %t6133, %L_1508 ], [ %t6105, %L_1513 ]
  %stackTop.63 = phi i8* [ %stackTop.120, %L_1508 ], [ %stackTop.62, %L_1513 ]
  %frontier.65 = phi i8* [ %frontier.119, %L_1508 ], [ %t6116, %L_1513 ]
  %t6125 = load i8*, i8** %t14203, align 8
  %t6127.not = icmp ult i8* %t6125, %frontier.65
  br i1 %t6127.not, label %L_1792, label %L_1512

L_1508:                                           ; preds = %L_1787
  %t6132 = bitcast i8* %TP_1.17 to i8**
  %t6133 = load i8*, i8** %t6132, align 8
  %t6136 = bitcast i8* %t6194 to i32*
  store i32 1, i32* %t6136, align 4
  %t6142 = getelementptr inbounds i8, i8* %stackTop.120, i64 84
  %t6143 = bitcast i8* %t6142 to i32*
  store i32 %TW32_0.32, i32* %t6143, align 4
  %t6149 = getelementptr inbounds i8, i8* %stackTop.120, i64 152
  %t6150 = bitcast i8* %t6149 to i8**
  store i8* %TP_0.56, i8** %t6150, align 8
  %t6156 = getelementptr inbounds i8, i8* %stackTop.120, i64 168
  %t6157 = bitcast i8* %t6156 to i8**
  store i8* %t6189, i8** %t6157, align 8
  %t6163 = getelementptr inbounds i8, i8* %stackTop.120, i64 176
  %t6164 = bitcast i8* %t6163 to i8**
  store i8* %t6196, i8** %t6164, align 8
  br label %L_1509

L_1787:                                           ; preds = %L_1504
  %t6172 = bitcast i8* %t6200 to i32*
  store i32 1, i32* %t6172, align 4
  %t6175 = getelementptr inbounds i8, i8* %stackTop.120, i64 76
  %t6176 = bitcast i8* %t6175 to i32*
  store i32 %TW32_0.32, i32* %t6176, align 4
  %t6180 = getelementptr inbounds i8, i8* %stackTop.120, i64 96
  %t6181 = bitcast i8* %t6180 to i8**
  store i8* %TP_0.56, i8** %t6181, align 8
  %t6184 = getelementptr inbounds i8, i8* %stackTop.120, i64 136
  %t6185 = bitcast i8* %t6184 to i8**
  %t6187 = getelementptr inbounds i8, i8* %stackTop.120, i64 64
  %t6188 = bitcast i8* %t6187 to i8**
  %t6189 = load i8*, i8** %t6188, align 8
  store i8* %t6189, i8** %t6185, align 8
  %t6191 = getelementptr inbounds i8, i8* %stackTop.120, i64 144
  %t6192 = bitcast i8* %t6191 to i8**
  %t6194 = getelementptr inbounds i8, i8* %stackTop.120, i64 80
  %t6195 = bitcast i8* %t6194 to i8**
  %t6196 = load i8*, i8** %t6195, align 8
  store i8* %t6196, i8** %t6192, align 8
  %cond17 = icmp eq i8* %TP_1.17, inttoptr (i64 1 to i8*)
  br i1 %cond17, label %L_1521, label %L_1508

L_1504:                                           ; preds = %L_1502
  %t6225 = getelementptr inbounds i8, i8* %TP_0.56, i64 %t6250
  %t6228 = load i8, i8* %t6225, align 1
  %t6232 = zext i8 %t6228 to i64
  %t6200 = getelementptr inbounds i8, i8* %stackTop.120, i64 72
  %t6201 = bitcast i8* %t6200 to i8**
  %t6202 = load i8*, i8** %t6201, align 8
  %t6204 = shl nuw nsw i64 %t6232, 2
  %t6205 = getelementptr inbounds i8, i8* %t6202, i64 %t6204
  %t6207 = bitcast i8* %t6205 to i32*
  %t6208 = load i32, i32* %t6207, align 4
  %switch2339 = icmp eq i32 %t6208, 0
  br i1 %switch2339, label %L_1787, label %L_1528

L_1503:                                           ; preds = %L_1502
  %t6238 = getelementptr inbounds i8, i8* %stackTop.120, i64 24
  %t6239 = bitcast i8* %t6238 to i8**
  %t6241 = getelementptr inbounds i8, i8* %stackTop.120, i64 160
  %t6242 = bitcast i8* %t6241 to i8**
  %t6243 = load i8*, i8** %t6242, align 8
  store i8* %t6243, i8** %t6239, align 8
  %t6245 = getelementptr inbounds i8, i8* %stackTop.120, i64 56
  br label %L_981

L_1502:                                           ; preds = %L_1123
  %t6250 = sext i32 %TW32_0.32 to i64
  %t6256.not = icmp ugt i64 %t11136, %t6250
  br i1 %t6256.not, label %L_1504, label %L_1503

L_1786:                                           ; preds = %L_1126
  %t6294 = getelementptr inbounds i8, i8* %stackTop.119, i64 184
  %t6295 = bitcast i8* %t6294 to i8**
  store i8* %TP_1.16, i8** %t6295, align 8
  %t6298 = getelementptr inbounds i8, i8* %stackTop.119, i64 192
  %t6299 = bitcast i8* %t6298 to i8**
  store i8* %TP_0.55, i8** %t6299, align 8
  %t6302 = getelementptr inbounds i8, i8* %stackTop.119, i64 200
  %t6303 = bitcast i8* %t6302 to i32*
  %t6305 = getelementptr inbounds i8, i8* %stackTop.119, i64 80
  %t6306 = bitcast i8* %t6305 to i32*
  %t6307 = load i32, i32* %t6306, align 4
  store i32 %t6307, i32* %t6303, align 4
  %t6309 = getelementptr inbounds i8, i8* %stackTop.119, i64 204
  %t6310 = bitcast i8* %t6309 to i32*
  %t6312 = getelementptr inbounds i8, i8* %stackTop.119, i64 84
  %t6313 = bitcast i8* %t6312 to i32*
  %t6314 = load i32, i32* %t6313, align 4
  store i32 %t6314, i32* %t6310, align 4
  %t6316 = getelementptr inbounds i8, i8* %stackTop.119, i64 208
  %t6317 = bitcast i8* %t6316 to i8**
  %t6319 = getelementptr inbounds i8, i8* %stackTop.119, i64 152
  %t6320 = bitcast i8* %t6319 to i8**
  %t6321 = load i8*, i8** %t6320, align 8
  store i8* %t6321, i8** %t6317, align 8
  %t6323 = getelementptr inbounds i8, i8* %stackTop.119, i64 216
  %t6324 = bitcast i8* %t6323 to i8**
  %t6326 = getelementptr inbounds i8, i8* %stackTop.119, i64 168
  %t6327 = bitcast i8* %t6326 to i8**
  %t6328 = load i8*, i8** %t6327, align 8
  store i8* %t6328, i8** %t6324, align 8
  %t6330 = getelementptr inbounds i8, i8* %stackTop.119, i64 224
  %t6331 = bitcast i8* %t6330 to i8**
  %t6333 = getelementptr inbounds i8, i8* %stackTop.119, i64 176
  %t6334 = bitcast i8* %t6333 to i8**
  %t6335 = load i8*, i8** %t6334, align 8
  store i8* %t6335, i8** %t6331, align 8
  %t6337 = getelementptr inbounds i8, i8* %stackTop.119, i64 232
  %t6338 = bitcast i8* %t6337 to i64*
  store i64 122, i64* %t6338, align 4
  %t6340 = getelementptr inbounds i8, i8* %stackTop.119, i64 240
  store i8* %frontier.118, i8** %t15197, align 8
  store i8* %t6340, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t6350 = load i8*, i8** %t15197, align 8
  %t6353 = load i8*, i8** %t15200, align 8
  %t6261 = getelementptr inbounds i8, i8* %t6353, i64 -240
  %t6263 = getelementptr inbounds i8, i8* %t6353, i64 -56
  %t6264 = bitcast i8* %t6263 to i8**
  %t6265 = load i8*, i8** %t6264, align 8
  %t6267 = getelementptr inbounds i8, i8* %t6353, i64 -48
  %t6268 = bitcast i8* %t6267 to i8**
  %t6269 = load i8*, i8** %t6268, align 8
  br label %L_1129

L_1499:                                           ; preds = %L_1137
  %t6355 = getelementptr inbounds i8, i8* %stackTop.118, i64 24
  %t6356 = bitcast i8* %t6355 to i8**
  %t6358 = getelementptr inbounds i8, i8* %stackTop.118, i64 160
  %t6359 = bitcast i8* %t6358 to i8**
  %t6360 = load i8*, i8** %t6359, align 8
  store i8* %t6360, i8** %t6356, align 8
  %t6362 = getelementptr inbounds i8, i8* %stackTop.118, i64 56
  br label %L_981

L_1498:                                           ; preds = %L_1496
  %t6367 = getelementptr inbounds i8, i8* %stackTop.1172622, i64 24
  %t6368 = bitcast i8* %t6367 to i8**
  %t6370 = getelementptr inbounds i8, i8* %stackTop.1172622, i64 160
  %t6371 = bitcast i8* %t6370 to i8**
  %t6372 = load i8*, i8** %t6371, align 8
  store i8* %t6372, i8** %t6368, align 8
  %t6374 = getelementptr inbounds i8, i8* %stackTop.1172622, i64 56
  br label %L_981

L_1497:                                           ; preds = %L_1496
  %t6382 = bitcast i8* %TP_0.523518 to i8**
  %t6383 = load i8*, i8** %t6382, align 8
  %t6385 = add nuw nsw i64 %TW64_1.223519, 1
  %t10922.not = icmp slt i64 %t6385, %t109462615
  br i1 %t10922.not, label %L_1496, label %L_1139

L_1496:                                           ; preds = %L_1138, %L_1497
  %TW64_1.223519 = phi i64 [ %t6385, %L_1497 ], [ 0, %L_1138 ]
  %TP_0.523518 = phi i8* [ %t6383, %L_1497 ], [ %TP_0.532620, %L_1138 ]
  %cond26 = icmp eq i8* %TP_0.523518, inttoptr (i64 1 to i8*)
  br i1 %cond26, label %L_1498, label %L_1497

L_1140:                                           ; preds = %L_1139
  %t6393 = getelementptr inbounds i8, i8* %stackTop.1172622.sink5218, i64 168
  %t6394 = bitcast i8* %t6393 to i64*
  store i64 98, i64* %t6394, align 4
  %t6396 = getelementptr inbounds i8, i8* %stackTop.1172622.sink5218, i64 176
  store i8* %frontier.115, i8** %t15197, align 8
  store i8* %t6396, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t6406 = load i8*, i8** %t15197, align 8
  %t6409 = load i8*, i8** %t15200, align 8
  %t6391 = getelementptr inbounds i8, i8* %t6409, i64 -176
  br label %L_1142

L_1494:                                           ; preds = %L_1493
  %t6446 = lshr i64 %TW64_0.13, 1
  %t6448 = trunc i64 %t6446 to i32
  br label %L_1152.preheader

L_1493:                                           ; preds = %doSwitchNextBlock, %doSwitchNextBlock
  %stackTop.64 = getelementptr inbounds i8, i8* %stackTop.0, i64 -176
  %TW64_0.13.in = bitcast i8* %stackTop.0 to i64*
  %TW64_0.13 = load i64, i64* %TW64_0.13.in, align 4
  %83 = and i64 %TW64_0.13, 1
  %trunc2129.not = icmp eq i64 %83, 0
  br i1 %trunc2129.not, label %L_1143, label %L_1494

L_1492:                                           ; preds = %L_1146
  br label %L_120.sink.split

L_1490:                                           ; preds = %L_1149
  %t6490 = getelementptr inbounds i8, i8* %stackTop.0, i64 -152
  %t6491 = bitcast i8* %t6490 to i8**
  %t6493 = getelementptr inbounds i8, i8* %stackTop.0, i64 -16
  %t6494 = bitcast i8* %t6493 to i8**
  %t6495 = load i8*, i8** %t6494, align 8
  store i8* %t6495, i8** %t6491, align 8
  %t6497 = getelementptr inbounds i8, i8* %stackTop.0, i64 -120
  br label %L_981

L_1488:                                           ; preds = %L_1170
  %t6502 = getelementptr inbounds i8, i8* %stackTop.111, i64 24
  %t6503 = bitcast i8* %t6502 to i8**
  %t6505 = getelementptr inbounds i8, i8* %stackTop.111, i64 160
  %t6506 = bitcast i8* %t6505 to i8**
  %t6507 = load i8*, i8** %t6506, align 8
  store i8* %t6507, i8** %t6503, align 8
  %t6509 = getelementptr inbounds i8, i8* %stackTop.111, i64 56
  br label %L_981

L_1487:                                           ; preds = %L_1485
  %t6514 = getelementptr inbounds i8, i8* %t10470, i64 -152
  %t6515 = bitcast i8* %t6514 to i8**
  %t6517 = getelementptr inbounds i8, i8* %t10470, i64 -16
  %t6518 = bitcast i8* %t6517 to i8**
  %t6519 = load i8*, i8** %t6518, align 8
  store i8* %t6519, i8** %t6515, align 8
  %t6521 = getelementptr inbounds i8, i8* %t10470, i64 -120
  br label %L_981

L_1486:                                           ; preds = %L_1485
  %t6528 = getelementptr inbounds i8, i8* %TP_0.503595, i64 8
  %t6529 = bitcast i8* %t6528 to i8**
  %t6530 = load i8*, i8** %t6529, align 8
  %t6533 = bitcast i8* %TP_0.503595 to i32*
  %t6534 = load i32, i32* %t6533, align 4
  %t6537 = shl nsw i64 %TW64_1.213596, 2
  %t6538 = getelementptr inbounds i8, i8* %t10464, i64 %t6537
  %t6540 = bitcast i8* %t6538 to i32*
  store i32 %t6534, i32* %t6540, align 4
  %t6543 = add nuw nsw i64 %TW64_1.213596, 1
  %t10430.not = icmp slt i64 %t6543, %t10440
  br i1 %t10430.not, label %L_1485, label %L_1750

L_1485:                                           ; preds = %L_1485.preheader, %L_1486
  %TW64_1.213596 = phi i64 [ %t6543, %L_1486 ], [ 0, %L_1485.preheader ]
  %TP_0.503595 = phi i8* [ %t6530, %L_1486 ], [ %t10448, %L_1485.preheader ]
  %cond31 = icmp eq i8* %TP_0.503595, inttoptr (i64 1 to i8*)
  br i1 %cond31, label %L_1487, label %L_1486

L_1174:                                           ; preds = %L_1750
  %t6551 = getelementptr inbounds i8, i8* %t10470, i64 -8
  %t6552 = bitcast i8* %t6551 to i64*
  store i64 121, i64* %t6552, align 4
  store i8* %t10467, i8** %t15197, align 8
  store i8* %t10470, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t6564 = load i8*, i8** %t15197, align 8
  %t6567 = load i8*, i8** %t15200, align 8
  %t6549 = getelementptr inbounds i8, i8* %t6567, i64 -176
  br label %L_1176

L_1484:                                           ; preds = %L_1176
  %t6569 = getelementptr inbounds i8, i8* %stackTop.110, i64 24
  %t6570 = bitcast i8* %t6569 to i8**
  %t6572 = getelementptr inbounds i8, i8* %stackTop.110, i64 160
  %t6573 = bitcast i8* %t6572 to i8**
  %t6574 = load i8*, i8** %t6573, align 8
  store i8* %t6574, i8** %t6570, align 8
  %t6576 = getelementptr inbounds i8, i8* %stackTop.110, i64 56
  br label %L_981

L_1783:                                           ; preds = %loop_77
  %t6591 = getelementptr inbounds i8, i8* %stackTop.70, i64 176
  %t6592 = bitcast i8* %t6591 to i8**
  %t6594 = getelementptr inbounds i8, i8* %stackTop.70, i64 64
  %t6595 = bitcast i8* %t6594 to i8**
  %t6596 = load i8*, i8** %t6595, align 8
  store i8* %t6596, i8** %t6592, align 8
  %t6598 = getelementptr inbounds i8, i8* %stackTop.70, i64 184
  %t6599 = bitcast i8* %t6598 to i8**
  %t6601 = getelementptr inbounds i8, i8* %stackTop.70, i64 152
  %t6602 = bitcast i8* %t6601 to i8**
  %t6603 = load i8*, i8** %t6602, align 8
  store i8* %t6603, i8** %t6599, align 8
  %t6605 = getelementptr inbounds i8, i8* %stackTop.70, i64 192
  %t6606 = bitcast i8* %t6605 to i64*
  store i64 120, i64* %t6606, align 4
  %t6608 = getelementptr inbounds i8, i8* %stackTop.70, i64 200
  store i8* %frontier.71, i8** %t15197, align 8
  store i8* %t6608, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t6618 = load i8*, i8** %t15197, align 8
  %t6621 = load i8*, i8** %t15200, align 8
  %t6581 = getelementptr inbounds i8, i8* %t6621, i64 -200
  br label %L_1468

L_1481:                                           ; preds = %L_1782
  %t6625 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t6626 = bitcast i8* %t6625 to i64*
  store i64 120, i64* %t6626, align 4
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %stackTop.0, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t6638 = load i8*, i8** %t15197, align 8
  %t6641 = load i8*, i8** %t15200, align 8
  br label %L_1483

L_1483:                                           ; preds = %L_1782, %L_1481
  %t6641.pn = phi i8* [ %t6641, %L_1481 ], [ %stackTop.0, %L_1782 ]
  %frontier.66 = phi i8* [ %t6638, %L_1481 ], [ %frontier.0, %L_1782 ]
  %stackTop.65 = getelementptr inbounds i8, i8* %t6641.pn, i64 -200
  %t6643 = getelementptr inbounds i8, i8* %frontier.66, i64 8
  %t6648 = bitcast i8* %frontier.66 to i64*
  store i64 85, i64* %t6648, align 4
  %t6650 = getelementptr inbounds i8, i8* %frontier.66, i64 16
  %t6653 = bitcast i8* %t6643 to i8**
  %t6655 = getelementptr inbounds i8, i8* %t6641.pn, i64 -112
  %t6656 = bitcast i8* %t6655 to i8**
  %t6657 = load i8*, i8** %t6656, align 8
  store i8* %t6657, i8** %t6653, align 8
  store i8* %t6643, i8** %t6656, align 8
  %t6671 = getelementptr inbounds i8, i8* %t6641.pn, i64 -32
  %t6672 = bitcast i8* %t6671 to i32*
  %t6674 = getelementptr inbounds i8, i8* %t6641.pn, i64 -28
  %t6675 = bitcast i8* %t6674 to i32*
  %t6676 = load i32, i32* %t6675, align 4
  store i32 %t6676, i32* %t6672, align 4
  br label %loop_77

L_1782:                                           ; preds = %doSwitchNextBlock
  %t6681 = load i8*, i8** %t14203, align 8
  %t6683.not = icmp ult i8* %t6681, %frontier.0
  br i1 %t6683.not, label %L_1481, label %L_1483

fromInt32Unsafe_3:                                ; preds = %doSwitchNextBlock
  %t6690 = bitcast i8* %stackTop.0 to i8**
  %t6692 = getelementptr inbounds i8, i8* %stackTop.0, i64 -24
  %t6693 = bitcast i8* %t6692 to i8**
  %t6694 = load i8*, i8** %t6693, align 8
  store i8* %t6694, i8** %t6690, align 8
  %t6696 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  %t6697 = bitcast i8* %t6696 to i8**
  %t6699 = getelementptr inbounds i8, i8* %stackTop.0, i64 -104
  %t6700 = bitcast i8* %t6699 to i8**
  %t6701 = load i8*, i8** %t6700, align 8
  store i8* %t6701, i8** %t6697, align 8
  %t6703 = getelementptr inbounds i8, i8* %stackTop.0, i64 16
  %t6704 = bitcast i8* %t6703 to i8**
  %t6706 = getelementptr inbounds i8, i8* %stackTop.0, i64 -64
  %t6707 = bitcast i8* %t6706 to i8**
  %t6708 = load i8*, i8** %t6707, align 8
  store i8* %t6708, i8** %t6704, align 8
  %t6710 = getelementptr inbounds i8, i8* %stackTop.0, i64 24
  %t6711 = bitcast i8* %t6710 to i8**
  %t6713 = getelementptr inbounds i8, i8* %stackTop.0, i64 -56
  %t6714 = bitcast i8* %t6713 to i8**
  %t6715 = load i8*, i8** %t6714, align 8
  store i8* %t6715, i8** %t6711, align 8
  %t6717 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  br label %L_81.sink.split

L_1781:                                           ; preds = %L_1468
  %t6722 = getelementptr inbounds i8, i8* %stackTop.69, i64 176
  %t6723 = bitcast i8* %t6722 to i8**
  %t6725 = getelementptr inbounds i8, i8* %stackTop.69, i64 64
  %t6726 = bitcast i8* %t6725 to i8**
  %t6727 = load i8*, i8** %t6726, align 8
  store i8* %t6727, i8** %t6723, align 8
  %t6729 = getelementptr inbounds i8, i8* %stackTop.69, i64 184
  %t6730 = bitcast i8* %t6729 to i8**
  %t6732 = getelementptr inbounds i8, i8* %stackTop.69, i64 152
  %t6733 = bitcast i8* %t6732 to i8**
  %t6734 = load i8*, i8** %t6733, align 8
  store i8* %t6734, i8** %t6730, align 8
  %t6739 = add nsw i32 %t6913, 1
  %t6741 = getelementptr inbounds i8, i8* %stackTop.69, i64 172
  %t6742 = bitcast i8* %t6741 to i32*
  store i32 %t6739, i32* %t6742, align 4
  %t6744 = getelementptr inbounds i8, i8* %stackTop.69, i64 200
  %t6745 = bitcast i8* %t6744 to i8**
  store i8* %t6727, i8** %t6745, align 8
  %t6751 = getelementptr inbounds i8, i8* %stackTop.69, i64 208
  %t6752 = bitcast i8* %t6751 to i8**
  %t6754 = getelementptr inbounds i8, i8* %stackTop.69, i64 96
  %t6755 = bitcast i8* %t6754 to i8**
  %t6756 = load i8*, i8** %t6755, align 8
  store i8* %t6756, i8** %t6752, align 8
  %t6758 = getelementptr inbounds i8, i8* %stackTop.69, i64 216
  %t6759 = bitcast i8* %t6758 to i8**
  %t6761 = getelementptr inbounds i8, i8* %stackTop.69, i64 136
  %t6762 = bitcast i8* %t6761 to i8**
  %t6763 = load i8*, i8** %t6762, align 8
  store i8* %t6763, i8** %t6759, align 8
  %t6765 = getelementptr inbounds i8, i8* %stackTop.69, i64 224
  %t6766 = bitcast i8* %t6765 to i8**
  %t6768 = getelementptr inbounds i8, i8* %stackTop.69, i64 144
  %t6769 = bitcast i8* %t6768 to i8**
  %t6770 = load i8*, i8** %t6769, align 8
  store i8* %t6770, i8** %t6766, align 8
  %t6772 = getelementptr inbounds i8, i8* %stackTop.69, i64 192
  br label %L_81.sink.split

L_1780:                                           ; preds = %L_1471
  %t6788 = getelementptr inbounds i8, i8* %stackTop.68, i64 176
  %t6789 = bitcast i8* %t6788 to i8**
  %t6791 = getelementptr inbounds i8, i8* %stackTop.68, i64 64
  %t6792 = bitcast i8* %t6791 to i8**
  %t6793 = load i8*, i8** %t6792, align 8
  store i8* %t6793, i8** %t6789, align 8
  %t6795 = getelementptr inbounds i8, i8* %stackTop.68, i64 184
  %t6796 = bitcast i8* %t6795 to i8**
  %t6798 = getelementptr inbounds i8, i8* %stackTop.68, i64 152
  %t6799 = bitcast i8* %t6798 to i8**
  %t6800 = load i8*, i8** %t6799, align 8
  store i8* %t6800, i8** %t6796, align 8
  %t6802 = getelementptr inbounds i8, i8* %stackTop.68, i64 192
  %t6803 = bitcast i8* %t6802 to i64*
  store i64 119, i64* %t6803, align 4
  %t6805 = getelementptr inbounds i8, i8* %stackTop.68, i64 200
  store i8* %frontier.69, i8** %t15197, align 8
  store i8* %t6805, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t6815 = load i8*, i8** %t15197, align 8
  %t6818 = load i8*, i8** %t15200, align 8
  %t6778 = getelementptr inbounds i8, i8* %t6818, i64 -200
  br label %L_1474

L_1477:                                           ; preds = %L_1474, %L_1469
  %TP_0.32 = phi i8* [ inttoptr (i64 1 to i8*), %L_1469 ], [ %t6863, %L_1474 ]
  %stackTop.66 = phi i8* [ %stackTop.69, %L_1469 ], [ %stackTop.67, %L_1474 ]
  %frontier.67 = phi i8* [ %frontier.70, %L_1469 ], [ %t6870, %L_1474 ]
  %t6820 = getelementptr inbounds i8, i8* %frontier.67, i64 8
  %t6825 = bitcast i8* %frontier.67 to i64*
  store i64 115, i64* %t6825, align 4
  %t6827 = getelementptr inbounds i8, i8* %frontier.67, i64 24
  %t6830 = bitcast i8* %t6820 to i8**
  store i8* %TP_0.32, i8** %t6830, align 8
  %t6833 = getelementptr inbounds i8, i8* %frontier.67, i64 16
  %t6834 = bitcast i8* %t6833 to i8**
  %t6836 = getelementptr inbounds i8, i8* %stackTop.66, i64 80
  %t6837 = bitcast i8* %t6836 to i8**
  %t6838 = load i8*, i8** %t6837, align 8
  store i8* %t6838, i8** %t6834, align 8
  store i8* %t6820, i8** %t6837, align 8
  %t6844 = getelementptr inbounds i8, i8* %stackTop.66, i64 76
  %t6845 = bitcast i8* %t6844 to i32*
  %t6846 = load i32, i32* %t6845, align 4
  %t10315.phi.trans.insert = getelementptr inbounds i8, i8* %stackTop.66, i64 72
  %t10316.phi.trans.insert = bitcast i8* %t10315.phi.trans.insert to i32*
  %t10317.pre = load i32, i32* %t10316.phi.trans.insert, align 4
  br label %loop_64

L_1475:                                           ; preds = %L_1474
  %t6852 = bitcast i8* %t6881 to i8**
  %t6853 = load i8*, i8** %t6852, align 8
  store i8* %t6853, i8** %t6880, align 8
  store i8* %t6863, i8** %t6876, align 8
  br label %L_1471

L_1474:                                           ; preds = %L_1471, %L_1780
  %stackTop.67 = phi i8* [ %t6778, %L_1780 ], [ %stackTop.68, %L_1471 ]
  %frontier.68 = phi i8* [ %t6815, %L_1780 ], [ %frontier.69, %L_1471 ]
  %t6863 = getelementptr inbounds i8, i8* %frontier.68, i64 8
  %t6868 = bitcast i8* %frontier.68 to i64*
  store i64 85, i64* %t6868, align 4
  %t6870 = getelementptr inbounds i8, i8* %frontier.68, i64 16
  %t6873 = bitcast i8* %t6863 to i8**
  %t6875 = getelementptr inbounds i8, i8* %stackTop.67, i64 168
  %t6876 = bitcast i8* %t6875 to i8**
  %t6877 = load i8*, i8** %t6876, align 8
  store i8* %t6877, i8** %t6873, align 8
  %t6879 = getelementptr inbounds i8, i8* %stackTop.67, i64 88
  %t6880 = bitcast i8* %t6879 to i8**
  %t6881 = load i8*, i8** %t6880, align 8
  %cond33 = icmp eq i8* %t6881, inttoptr (i64 1 to i8*)
  br i1 %cond33, label %L_1477, label %L_1475

L_1471:                                           ; preds = %L_1470, %L_1475
  %stackTop.68 = phi i8* [ %stackTop.69, %L_1470 ], [ %stackTop.67, %L_1475 ]
  %frontier.69 = phi i8* [ %frontier.70, %L_1470 ], [ %t6870, %L_1475 ]
  %t6885 = load i8*, i8** %t14203, align 8
  %t6887.not = icmp ult i8* %t6885, %frontier.69
  br i1 %t6887.not, label %L_1780, label %L_1474

L_1470:                                           ; preds = %L_1469
  %t6895 = bitcast i8* %t6908 to i8**
  %t6896 = load i8*, i8** %t6895, align 8
  store i8* %t6896, i8** %t6907, align 8
  %t6903 = bitcast i8* %t6911 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t6903, align 8
  br label %L_1471

L_1469:                                           ; preds = %L_1468
  %t6906 = getelementptr inbounds i8, i8* %stackTop.69, i64 88
  %t6907 = bitcast i8* %t6906 to i8**
  %t6908 = load i8*, i8** %t6907, align 8
  %cond32 = icmp eq i8* %t6908, inttoptr (i64 1 to i8*)
  br i1 %cond32, label %L_1477, label %L_1470

L_1468:                                           ; preds = %loop_77, %L_1783
  %stackTop.69 = phi i8* [ %t6581, %L_1783 ], [ %stackTop.70, %loop_77 ]
  %frontier.70 = phi i8* [ %t6618, %L_1783 ], [ %frontier.71, %loop_77 ]
  %t6911 = getelementptr inbounds i8, i8* %stackTop.69, i64 168
  %t6912 = bitcast i8* %t6911 to i32*
  %t6913 = load i32, i32* %t6912, align 4
  %t6915 = getelementptr inbounds i8, i8* %stackTop.69, i64 72
  %t6916 = bitcast i8* %t6915 to i32*
  %t6917 = load i32, i32* %t6916, align 4
  %t6918.not = icmp slt i32 %t6913, %t6917
  br i1 %t6918.not, label %L_1781, label %L_1469

loop_77:                                          ; preds = %L_1465, %L_1483
  %stackTop.70 = phi i8* [ %stackTop.109, %L_1465 ], [ %stackTop.65, %L_1483 ]
  %frontier.71 = phi i8* [ %frontier.109, %L_1465 ], [ %t6650, %L_1483 ]
  %t6923 = load i8*, i8** %t14203, align 8
  %t6925.not = icmp ult i8* %t6923, %frontier.71
  br i1 %t6925.not, label %L_1783, label %L_1468

L_1465:                                           ; preds = %loop_64
  %t6929 = add nsw i32 %TW32_0.24, 1
  %t6931 = getelementptr inbounds i8, i8* %stackTop.109, i64 76
  %t6932 = bitcast i8* %t6931 to i32*
  store i32 %t6929, i32* %t6932, align 4
  %t6934 = getelementptr inbounds i8, i8* %stackTop.109, i64 88
  %t6935 = bitcast i8* %t6934 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t6935, align 8
  %t6938 = getelementptr inbounds i8, i8* %stackTop.109, i64 168
  %t6939 = bitcast i8* %t6938 to i32*
  store i32 0, i32* %t6939, align 4
  br label %loop_77

L_1464:                                           ; preds = %L_1751
  store i8* inttoptr (i64 1 to i8*), i8** %t10299, align 8
  br label %L_1186

L_1779:                                           ; preds = %L_1180
  %t6970 = getelementptr inbounds i8, i8* %stackTop.108, i64 80
  %t6971 = bitcast i8* %t6970 to i8**
  store i8* %TP_2.6, i8** %t6971, align 8
  %t6974 = getelementptr inbounds i8, i8* %stackTop.108, i64 176
  %t6975 = bitcast i8* %t6974 to i8**
  store i8* %TP_1.13, i8** %t6975, align 8
  %t6978 = getelementptr inbounds i8, i8* %stackTop.108, i64 184
  %t6979 = bitcast i8* %t6978 to i8**
  store i8* %TP_0.49, i8** %t6979, align 8
  %t6982 = getelementptr inbounds i8, i8* %stackTop.108, i64 192
  %t6983 = bitcast i8* %t6982 to i8**
  %t6985 = getelementptr inbounds i8, i8* %stackTop.108, i64 64
  %t6986 = bitcast i8* %t6985 to i8**
  %t6987 = load i8*, i8** %t6986, align 8
  store i8* %t6987, i8** %t6983, align 8
  %t6989 = getelementptr inbounds i8, i8* %stackTop.108, i64 200
  %t6990 = bitcast i8* %t6989 to i8**
  %t6992 = getelementptr inbounds i8, i8* %stackTop.108, i64 152
  %t6993 = bitcast i8* %t6992 to i8**
  %t6994 = load i8*, i8** %t6993, align 8
  store i8* %t6994, i8** %t6990, align 8
  %t6996 = getelementptr inbounds i8, i8* %stackTop.108, i64 208
  %t6997 = bitcast i8* %t6996 to i64*
  store i64 118, i64* %t6997, align 4
  %t6999 = getelementptr inbounds i8, i8* %stackTop.108, i64 216
  store i8* %frontier.108, i8** %t15197, align 8
  store i8* %t6999, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t7009 = load i8*, i8** %t15197, align 8
  %t7012 = load i8*, i8** %t15200, align 8
  %t6945 = getelementptr inbounds i8, i8* %t7012, i64 -216
  %t6947 = getelementptr inbounds i8, i8* %t7012, i64 -136
  %t6948 = bitcast i8* %t6947 to i8**
  %t6949 = load i8*, i8** %t6948, align 8
  %t6951 = getelementptr inbounds i8, i8* %t7012, i64 -40
  %t6952 = bitcast i8* %t6951 to i8**
  %t6953 = load i8*, i8** %t6952, align 8
  %t6955 = getelementptr inbounds i8, i8* %t7012, i64 -32
  %t6956 = bitcast i8* %t6955 to i8**
  %t6957 = load i8*, i8** %t6956, align 8
  br label %L_1183

L_1778:                                           ; preds = %loop_76
  %t7024 = getelementptr inbounds i8, i8* %stackTop.76, i64 184
  %t7025 = bitcast i8* %t7024 to i8**
  %t7027 = getelementptr inbounds i8, i8* %stackTop.76, i64 80
  %t7028 = bitcast i8* %t7027 to i8**
  %t7029 = load i8*, i8** %t7028, align 8
  store i8* %t7029, i8** %t7025, align 8
  %t7031 = getelementptr inbounds i8, i8* %stackTop.76, i64 192
  %t7032 = bitcast i8* %t7031 to i8**
  %t7034 = getelementptr inbounds i8, i8* %stackTop.76, i64 152
  %t7035 = bitcast i8* %t7034 to i8**
  %t7036 = load i8*, i8** %t7035, align 8
  store i8* %t7036, i8** %t7032, align 8
  %t7038 = getelementptr inbounds i8, i8* %stackTop.76, i64 200
  %t7039 = bitcast i8* %t7038 to i64*
  store i64 117, i64* %t7039, align 4
  %t7041 = getelementptr inbounds i8, i8* %stackTop.76, i64 208
  store i8* %frontier.77, i8** %t15197, align 8
  store i8* %t7041, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t7051 = load i8*, i8** %t15197, align 8
  %t7054 = load i8*, i8** %t15200, align 8
  %t7014 = getelementptr inbounds i8, i8* %t7054, i64 -208
  br label %L_1448

L_1461:                                           ; preds = %L_1777
  %t7058 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t7059 = bitcast i8* %t7058 to i64*
  store i64 117, i64* %t7059, align 4
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %stackTop.0, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t7071 = load i8*, i8** %t15197, align 8
  %t7074 = load i8*, i8** %t15200, align 8
  br label %L_1463

L_1463:                                           ; preds = %L_1777, %L_1461
  %t7074.pn = phi i8* [ %t7074, %L_1461 ], [ %stackTop.0, %L_1777 ]
  %frontier.72 = phi i8* [ %t7071, %L_1461 ], [ %frontier.0, %L_1777 ]
  %stackTop.71 = getelementptr inbounds i8, i8* %t7074.pn, i64 -208
  %t7076 = getelementptr inbounds i8, i8* %frontier.72, i64 8
  %t7081 = bitcast i8* %frontier.72 to i64*
  store i64 85, i64* %t7081, align 4
  %t7083 = getelementptr inbounds i8, i8* %frontier.72, i64 16
  %t7086 = bitcast i8* %t7076 to i8**
  %t7088 = getelementptr inbounds i8, i8* %t7074.pn, i64 -40
  %t7089 = bitcast i8* %t7088 to i8**
  %t7090 = load i8*, i8** %t7089, align 8
  store i8* %t7090, i8** %t7086, align 8
  store i8* %t7076, i8** %t7089, align 8
  %t7104 = getelementptr inbounds i8, i8* %t7074.pn, i64 -32
  %t7105 = bitcast i8* %t7104 to i32*
  %t7107 = getelementptr inbounds i8, i8* %t7074.pn, i64 -28
  %t7108 = bitcast i8* %t7107 to i32*
  %t7109 = load i32, i32* %t7108, align 4
  store i32 %t7109, i32* %t7105, align 4
  br label %loop_76

L_1777:                                           ; preds = %doSwitchNextBlock
  %t7114 = load i8*, i8** %t14203, align 8
  %t7116.not = icmp ult i8* %t7114, %frontier.0
  br i1 %t7116.not, label %L_1461, label %L_1463

fromInt32Unsafe_2:                                ; preds = %doSwitchNextBlock
  %t7123 = bitcast i8* %stackTop.0 to i8**
  %t7125 = getelementptr inbounds i8, i8* %stackTop.0, i64 -24
  %t7126 = bitcast i8* %t7125 to i8**
  %t7127 = load i8*, i8** %t7126, align 8
  store i8* %t7127, i8** %t7123, align 8
  %t7129 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  %t7130 = bitcast i8* %t7129 to i8**
  %t7132 = getelementptr inbounds i8, i8* %stackTop.0, i64 -112
  %t7133 = bitcast i8* %t7132 to i8**
  %t7134 = load i8*, i8** %t7133, align 8
  store i8* %t7134, i8** %t7130, align 8
  %t7136 = getelementptr inbounds i8, i8* %stackTop.0, i64 16
  %t7137 = bitcast i8* %t7136 to i8**
  %t7139 = getelementptr inbounds i8, i8* %stackTop.0, i64 -72
  %t7140 = bitcast i8* %t7139 to i8**
  %t7141 = load i8*, i8** %t7140, align 8
  store i8* %t7141, i8** %t7137, align 8
  %t7143 = getelementptr inbounds i8, i8* %stackTop.0, i64 24
  %t7144 = bitcast i8* %t7143 to i8**
  %t7146 = getelementptr inbounds i8, i8* %stackTop.0, i64 -64
  %t7147 = bitcast i8* %t7146 to i8**
  %t7148 = load i8*, i8** %t7147, align 8
  store i8* %t7148, i8** %t7144, align 8
  %t7150 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  br label %L_81.sink.split

L_1776:                                           ; preds = %L_1448
  %t7155 = getelementptr inbounds i8, i8* %stackTop.75, i64 184
  %t7156 = bitcast i8* %t7155 to i8**
  %t7158 = getelementptr inbounds i8, i8* %stackTop.75, i64 80
  %t7159 = bitcast i8* %t7158 to i8**
  %t7160 = load i8*, i8** %t7159, align 8
  store i8* %t7160, i8** %t7156, align 8
  %t7162 = getelementptr inbounds i8, i8* %stackTop.75, i64 192
  %t7163 = bitcast i8* %t7162 to i8**
  %t7165 = getelementptr inbounds i8, i8* %stackTop.75, i64 152
  %t7166 = bitcast i8* %t7165 to i8**
  %t7167 = load i8*, i8** %t7166, align 8
  store i8* %t7167, i8** %t7163, align 8
  %t7172 = add nsw i32 %t7346, 1
  %t7174 = getelementptr inbounds i8, i8* %stackTop.75, i64 180
  %t7175 = bitcast i8* %t7174 to i32*
  store i32 %t7172, i32* %t7175, align 4
  %t7177 = getelementptr inbounds i8, i8* %stackTop.75, i64 208
  %t7178 = bitcast i8* %t7177 to i8**
  store i8* %t7160, i8** %t7178, align 8
  %t7184 = getelementptr inbounds i8, i8* %stackTop.75, i64 216
  %t7185 = bitcast i8* %t7184 to i8**
  %t7187 = getelementptr inbounds i8, i8* %stackTop.75, i64 96
  %t7188 = bitcast i8* %t7187 to i8**
  %t7189 = load i8*, i8** %t7188, align 8
  store i8* %t7189, i8** %t7185, align 8
  %t7191 = getelementptr inbounds i8, i8* %stackTop.75, i64 224
  %t7192 = bitcast i8* %t7191 to i8**
  %t7194 = getelementptr inbounds i8, i8* %stackTop.75, i64 136
  %t7195 = bitcast i8* %t7194 to i8**
  %t7196 = load i8*, i8** %t7195, align 8
  store i8* %t7196, i8** %t7192, align 8
  %t7198 = getelementptr inbounds i8, i8* %stackTop.75, i64 232
  %t7199 = bitcast i8* %t7198 to i8**
  %t7201 = getelementptr inbounds i8, i8* %stackTop.75, i64 144
  %t7202 = bitcast i8* %t7201 to i8**
  %t7203 = load i8*, i8** %t7202, align 8
  store i8* %t7203, i8** %t7199, align 8
  %t7205 = getelementptr inbounds i8, i8* %stackTop.75, i64 200
  br label %L_81.sink.split

L_1775:                                           ; preds = %L_1451
  %t7221 = getelementptr inbounds i8, i8* %stackTop.74, i64 184
  %t7222 = bitcast i8* %t7221 to i8**
  %t7224 = getelementptr inbounds i8, i8* %stackTop.74, i64 80
  %t7225 = bitcast i8* %t7224 to i8**
  %t7226 = load i8*, i8** %t7225, align 8
  store i8* %t7226, i8** %t7222, align 8
  %t7228 = getelementptr inbounds i8, i8* %stackTop.74, i64 192
  %t7229 = bitcast i8* %t7228 to i8**
  %t7231 = getelementptr inbounds i8, i8* %stackTop.74, i64 152
  %t7232 = bitcast i8* %t7231 to i8**
  %t7233 = load i8*, i8** %t7232, align 8
  store i8* %t7233, i8** %t7229, align 8
  %t7235 = getelementptr inbounds i8, i8* %stackTop.74, i64 200
  %t7236 = bitcast i8* %t7235 to i64*
  store i64 116, i64* %t7236, align 4
  %t7238 = getelementptr inbounds i8, i8* %stackTop.74, i64 208
  store i8* %frontier.75, i8** %t15197, align 8
  store i8* %t7238, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t7248 = load i8*, i8** %t15197, align 8
  %t7251 = load i8*, i8** %t15200, align 8
  %t7211 = getelementptr inbounds i8, i8* %t7251, i64 -208
  br label %L_1454

L_1457:                                           ; preds = %L_1454, %L_1449
  %TP_0.33 = phi i8* [ inttoptr (i64 1 to i8*), %L_1449 ], [ %t7296, %L_1454 ]
  %stackTop.72 = phi i8* [ %stackTop.75, %L_1449 ], [ %stackTop.73, %L_1454 ]
  %frontier.73 = phi i8* [ %frontier.76, %L_1449 ], [ %t7303, %L_1454 ]
  %t7253 = getelementptr inbounds i8, i8* %frontier.73, i64 8
  %t7258 = bitcast i8* %frontier.73 to i64*
  store i64 109, i64* %t7258, align 4
  %t7260 = getelementptr inbounds i8, i8* %frontier.73, i64 24
  %t7263 = bitcast i8* %t7253 to i8**
  store i8* %TP_0.33, i8** %t7263, align 8
  %t7266 = getelementptr inbounds i8, i8* %frontier.73, i64 16
  %t7267 = bitcast i8* %t7266 to i8**
  %t7269 = getelementptr inbounds i8, i8* %stackTop.72, i64 88
  %t7270 = bitcast i8* %t7269 to i8**
  %t7271 = load i8*, i8** %t7270, align 8
  store i8* %t7271, i8** %t7267, align 8
  store i8* %t7253, i8** %t7270, align 8
  %t7277 = getelementptr inbounds i8, i8* %stackTop.72, i64 76
  %t7278 = bitcast i8* %t7277 to i32*
  %t7279 = load i32, i32* %t7278, align 4
  br label %loop_65

L_1455:                                           ; preds = %L_1454
  %t7285 = bitcast i8* %t7314 to i8**
  %t7286 = load i8*, i8** %t7285, align 8
  store i8* %t7286, i8** %t7313, align 8
  store i8* %t7296, i8** %t7309, align 8
  br label %L_1451

L_1454:                                           ; preds = %L_1451, %L_1775
  %stackTop.73 = phi i8* [ %t7211, %L_1775 ], [ %stackTop.74, %L_1451 ]
  %frontier.74 = phi i8* [ %t7248, %L_1775 ], [ %frontier.75, %L_1451 ]
  %t7296 = getelementptr inbounds i8, i8* %frontier.74, i64 8
  %t7301 = bitcast i8* %frontier.74 to i64*
  store i64 85, i64* %t7301, align 4
  %t7303 = getelementptr inbounds i8, i8* %frontier.74, i64 16
  %t7306 = bitcast i8* %t7296 to i8**
  %t7308 = getelementptr inbounds i8, i8* %stackTop.73, i64 176
  %t7309 = bitcast i8* %t7308 to i8**
  %t7310 = load i8*, i8** %t7309, align 8
  store i8* %t7310, i8** %t7306, align 8
  %t7312 = getelementptr inbounds i8, i8* %stackTop.73, i64 168
  %t7313 = bitcast i8* %t7312 to i8**
  %t7314 = load i8*, i8** %t7313, align 8
  %cond37 = icmp eq i8* %t7314, inttoptr (i64 1 to i8*)
  br i1 %cond37, label %L_1457, label %L_1455

L_1451:                                           ; preds = %L_1450, %L_1455
  %stackTop.74 = phi i8* [ %stackTop.75, %L_1450 ], [ %stackTop.73, %L_1455 ]
  %frontier.75 = phi i8* [ %frontier.76, %L_1450 ], [ %t7303, %L_1455 ]
  %t7318 = load i8*, i8** %t14203, align 8
  %t7320.not = icmp ult i8* %t7318, %frontier.75
  br i1 %t7320.not, label %L_1775, label %L_1454

L_1450:                                           ; preds = %L_1449
  %t7328 = bitcast i8* %t7341 to i8**
  %t7329 = load i8*, i8** %t7328, align 8
  store i8* %t7329, i8** %t7340, align 8
  %t7336 = bitcast i8* %t7344 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t7336, align 8
  br label %L_1451

L_1449:                                           ; preds = %L_1448
  %t7339 = getelementptr inbounds i8, i8* %stackTop.75, i64 168
  %t7340 = bitcast i8* %t7339 to i8**
  %t7341 = load i8*, i8** %t7340, align 8
  %cond36 = icmp eq i8* %t7341, inttoptr (i64 1 to i8*)
  br i1 %cond36, label %L_1457, label %L_1450

L_1448:                                           ; preds = %loop_76, %L_1778
  %stackTop.75 = phi i8* [ %t7014, %L_1778 ], [ %stackTop.76, %loop_76 ]
  %frontier.76 = phi i8* [ %t7051, %L_1778 ], [ %frontier.77, %loop_76 ]
  %t7344 = getelementptr inbounds i8, i8* %stackTop.75, i64 176
  %t7345 = bitcast i8* %t7344 to i32*
  %t7346 = load i32, i32* %t7345, align 4
  %t7348 = getelementptr inbounds i8, i8* %stackTop.75, i64 72
  %t7349 = bitcast i8* %t7348 to i32*
  %t7350 = load i32, i32* %t7349, align 4
  %t7351.not = icmp slt i32 %t7346, %t7350
  br i1 %t7351.not, label %L_1776, label %L_1449

loop_76:                                          ; preds = %L_1445, %L_1463
  %stackTop.76 = phi i8* [ %stackTop.105, %L_1445 ], [ %stackTop.71, %L_1463 ]
  %frontier.77 = phi i8* [ %frontier.105, %L_1445 ], [ %t7083, %L_1463 ]
  %t7356 = load i8*, i8** %t14203, align 8
  %t7358.not = icmp ult i8* %t7356, %frontier.77
  br i1 %t7358.not, label %L_1778, label %L_1448

L_1445:                                           ; preds = %loop_65
  %t7362 = add nsw i32 %TW32_0.23, 1
  %t7364 = getelementptr inbounds i8, i8* %stackTop.105, i64 76
  %t7365 = bitcast i8* %t7364 to i32*
  store i32 %t7362, i32* %t7365, align 4
  %t7367 = getelementptr inbounds i8, i8* %stackTop.105, i64 168
  %t7368 = bitcast i8* %t7367 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t7368, align 8
  %t7371 = getelementptr inbounds i8, i8* %stackTop.105, i64 176
  %t7372 = bitcast i8* %t7371 to i32*
  store i32 0, i32* %t7372, align 4
  br label %loop_76

L_1444:                                           ; preds = %L_1754
  %t10186 = getelementptr inbounds i8, i8* %stackTop.105, i64 88
  %t10187 = bitcast i8* %t10186 to i8**
  %t7374 = getelementptr inbounds i8, i8* %stackTop.105, i64 80
  %t7375 = bitcast i8* %t7374 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t7375, align 8
  store i8* %t10184, i8** %t10187, align 8
  br label %L_1195

L_1774:                                           ; preds = %L_1189
  %t7403 = getelementptr inbounds i8, i8* %stackTop.104, i64 88
  %t7404 = bitcast i8* %t7403 to i8**
  store i8* %TP_2.4, i8** %t7404, align 8
  %t7407 = getelementptr inbounds i8, i8* %stackTop.104, i64 96
  %t7408 = bitcast i8* %t7407 to i8**
  store i8* %TP_1.11, i8** %t7408, align 8
  %t7411 = getelementptr inbounds i8, i8* %stackTop.104, i64 136
  %t7412 = bitcast i8* %t7411 to i8**
  store i8* %TP_0.47, i8** %t7412, align 8
  %t7415 = getelementptr inbounds i8, i8* %stackTop.104, i64 144
  %t7416 = bitcast i8* %t7415 to i8**
  %t7418 = getelementptr inbounds i8, i8* %stackTop.104, i64 80
  %t7419 = bitcast i8* %t7418 to i8**
  %t7420 = load i8*, i8** %t7419, align 8
  store i8* %t7420, i8** %t7416, align 8
  %t7422 = getelementptr inbounds i8, i8* %stackTop.104, i64 168
  %t7423 = bitcast i8* %t7422 to i64*
  store i64 115, i64* %t7423, align 4
  %t7425 = getelementptr inbounds i8, i8* %stackTop.104, i64 176
  store i8* %frontier.104, i8** %t15197, align 8
  store i8* %t7425, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t7435 = load i8*, i8** %t15197, align 8
  %t7438 = load i8*, i8** %t15200, align 8
  %t7382 = getelementptr inbounds i8, i8* %t7438, i64 -176
  %t7384 = getelementptr inbounds i8, i8* %t7438, i64 -88
  %t7385 = bitcast i8* %t7384 to i8**
  %t7386 = load i8*, i8** %t7385, align 8
  %t7388 = getelementptr inbounds i8, i8* %t7438, i64 -80
  %t7389 = bitcast i8* %t7388 to i8**
  %t7390 = load i8*, i8** %t7389, align 8
  %t7392 = getelementptr inbounds i8, i8* %t7438, i64 -40
  %t7393 = bitcast i8* %t7392 to i8**
  %t7394 = load i8*, i8** %t7393, align 8
  br label %L_1192

L_1441:                                           ; preds = %L_1757
  %t7453 = tail call i32 @IntInf_compare(i8* %gcState, i8* %t10080, i8* %t10074)
  %t7440 = icmp slt i32 %t7453, 1
  br i1 %t7440, label %L_1200, label %L_1441.L_1440_crit_edge

L_1441.L_1440_crit_edge:                          ; preds = %L_1441
  %t7460.pre = load i8*, i8** %t10079, align 8
  br label %L_1440

L_1440:                                           ; preds = %L_1441.L_1440_crit_edge, %L_1197
  %t7460 = phi i8* [ %t7460.pre, %L_1441.L_1440_crit_edge ], [ %t10080, %L_1197 ]
  br label %L_1200

L_1439:                                           ; preds = %L_1406
  %t7462 = getelementptr inbounds i8, i8* %stackTop.843552, i64 24
  %t7463 = bitcast i8* %t7462 to i8**
  %t7465 = getelementptr inbounds i8, i8* %stackTop.843552, i64 160
  %t7466 = bitcast i8* %t7465 to i8**
  %t7467 = load i8*, i8** %t7466, align 8
  store i8* %t7467, i8** %t7463, align 8
  %t7469 = getelementptr inbounds i8, i8* %stackTop.843552, i64 56
  br label %L_981

L_1438:                                           ; preds = %L_1407, %L_1436
  %t7474 = getelementptr inbounds i8, i8* %stackTop.843552, i64 24
  %t7475 = bitcast i8* %t7474 to i8**
  %t7477 = getelementptr inbounds i8, i8* %stackTop.843552, i64 160
  %t7478 = bitcast i8* %t7477 to i8**
  %t7479 = load i8*, i8** %t7478, align 8
  store i8* %t7479, i8** %t7475, align 8
  %t7481 = getelementptr inbounds i8, i8* %stackTop.843552, i64 56
  br label %L_981

L_1436:                                           ; preds = %L_1409.preheader, %L_1409
  %TP_0.373528.pn = phi i8* [ %TP_0.373528, %L_1409 ], [ %t7730, %L_1409.preheader ]
  %TW32_1.93527 = phi i32 [ %t7497, %L_1409 ], [ %t7740, %L_1409.preheader ]
  %TP_0.373528.in.in = getelementptr inbounds i8, i8* %TP_0.373528.pn, i64 8
  %TP_0.373528.in = bitcast i8* %TP_0.373528.in.in to i8**
  %TP_0.373528 = load i8*, i8** %TP_0.373528.in, align 8
  %cond41 = icmp eq i8* %TP_0.373528, inttoptr (i64 1 to i8*)
  br i1 %cond41, label %L_1438, label %L_1409

L_1435:                                           ; preds = %L_1410
  %t7501 = getelementptr inbounds i8, i8* %stackTop.843552, i64 24
  %t7502 = bitcast i8* %t7501 to i8**
  %t7504 = getelementptr inbounds i8, i8* %stackTop.843552, i64 160
  %t7505 = bitcast i8* %t7504 to i8**
  %t7506 = load i8*, i8** %t7505, align 8
  store i8* %t7506, i8** %t7502, align 8
  %t7508 = getelementptr inbounds i8, i8* %stackTop.843552, i64 56
  br label %L_981

L_1434:                                           ; preds = %L_1411, %L_1432
  %t7513 = getelementptr inbounds i8, i8* %stackTop.843552, i64 24
  %t7514 = bitcast i8* %t7513 to i8**
  %t7516 = getelementptr inbounds i8, i8* %stackTop.843552, i64 160
  %t7517 = bitcast i8* %t7516 to i8**
  %t7518 = load i8*, i8** %t7517, align 8
  store i8* %t7518, i8** %t7514, align 8
  %t7520 = getelementptr inbounds i8, i8* %stackTop.843552, i64 56
  br label %L_981

L_1432:                                           ; preds = %L_1413.preheader, %L_1413
  %TP_0.363534.in.in = phi i8* [ %TP_0.363534, %L_1413 ], [ %TP_1.9.le, %L_1413.preheader ]
  %TW32_1.83533 = phi i32 [ %t7531, %L_1413 ], [ %TW32_0.203551, %L_1413.preheader ]
  %TP_0.363534.in = bitcast i8* %TP_0.363534.in.in to i8**
  %TP_0.363534 = load i8*, i8** %TP_0.363534.in, align 8
  %cond43 = icmp eq i8* %TP_0.363534, inttoptr (i64 1 to i8*)
  br i1 %cond43, label %L_1434, label %L_1413

L_1431:                                           ; preds = %L_1414.thread, %L_1414, %L_1429
  %t7535 = getelementptr inbounds i8, i8* %stackTop.843552, i64 24
  %t7536 = bitcast i8* %t7535 to i8**
  %t7538 = getelementptr inbounds i8, i8* %stackTop.843552, i64 160
  %t7539 = bitcast i8* %t7538 to i8**
  %t7540 = load i8*, i8** %t7539, align 8
  store i8* %t7540, i8** %t7536, align 8
  %t7542 = getelementptr inbounds i8, i8* %stackTop.843552, i64 56
  br label %L_981

L_1429:                                           ; preds = %L_1416.preheader, %L_1416
  %TP_0.353540.pn = phi i8* [ %TP_0.353540, %L_1416 ], [ %t7688, %L_1416.preheader ]
  %TW32_0.193539 = phi i32 [ %t7558, %L_1416 ], [ %TW32_0.203551, %L_1416.preheader ]
  %TP_0.353540.in.in = getelementptr inbounds i8, i8* %TP_0.353540.pn, i64 8
  %TP_0.353540.in = bitcast i8* %TP_0.353540.in.in to i8**
  %TP_0.353540 = load i8*, i8** %TP_0.353540.in, align 8
  %cond45 = icmp eq i8* %TP_0.353540, inttoptr (i64 1 to i8*)
  br i1 %cond45, label %L_1431, label %L_1416

L_1428:                                           ; preds = %L_1417
  %t7562 = getelementptr inbounds i8, i8* %stackTop.843552, i64 24
  %t7563 = bitcast i8* %t7562 to i8**
  %t7565 = getelementptr inbounds i8, i8* %stackTop.843552, i64 160
  %t7566 = bitcast i8* %t7565 to i8**
  %t7567 = load i8*, i8** %t7566, align 8
  store i8* %t7567, i8** %t7563, align 8
  %t7569 = getelementptr inbounds i8, i8* %stackTop.843552, i64 56
  br label %L_981

L_1427:                                           ; preds = %L_1418, %L_1425
  %t7574 = getelementptr inbounds i8, i8* %stackTop.843552, i64 24
  %t7575 = bitcast i8* %t7574 to i8**
  %t7577 = getelementptr inbounds i8, i8* %stackTop.843552, i64 160
  %t7578 = bitcast i8* %t7577 to i8**
  %t7579 = load i8*, i8** %t7578, align 8
  store i8* %t7579, i8** %t7575, align 8
  %t7581 = getelementptr inbounds i8, i8* %stackTop.843552, i64 56
  br label %L_981

L_1425:                                           ; preds = %L_1420.preheader, %L_1420
  %TP_0.343546.in.in = phi i8* [ %TP_0.343546, %L_1420 ], [ %TP_1.8.le, %L_1420.preheader ]
  %TW32_0.183545 = phi i32 [ %t7592, %L_1420 ], [ %t7663, %L_1420.preheader ]
  %TP_0.343546.in = bitcast i8* %TP_0.343546.in.in to i8**
  %TP_0.343546 = load i8*, i8** %TP_0.343546.in, align 8
  %cond47 = icmp eq i8* %TP_0.343546, inttoptr (i64 1 to i8*)
  br i1 %cond47, label %L_1427, label %L_1420

L_1422:                                           ; preds = %L_1421
  %t7598 = getelementptr inbounds i8, i8* %stackTop.843552, i64 176
  %t7599 = bitcast i8* %t7598 to i64*
  store i64 114, i64* %t7599, align 4
  %t7601 = getelementptr inbounds i8, i8* %stackTop.843552, i64 184
  store i8* %frontier.853554, i8** %t15197, align 8
  store i8* %t7601, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t7611 = load i8*, i8** %t15197, align 8
  %t7614 = load i8*, i8** %t15200, align 8
  %t7596 = getelementptr inbounds i8, i8* %t7614, i64 -184
  br label %L_1424

L_1424:                                           ; preds = %L_1421, %L_1422
  %stackTop.77 = phi i8* [ %t7596, %L_1422 ], [ %stackTop.843552, %L_1421 ]
  %frontier.78 = phi i8* [ %t7611, %L_1422 ], [ %frontier.853554, %L_1421 ]
  %t7616 = getelementptr inbounds i8, i8* %frontier.78, i64 8
  %t7621 = bitcast i8* %frontier.78 to i64*
  store i64 85, i64* %t7621, align 4
  %t7623 = getelementptr inbounds i8, i8* %frontier.78, i64 16
  %t7626 = bitcast i8* %t7616 to i8**
  %t7628 = getelementptr inbounds i8, i8* %stackTop.77, i64 168
  %t7629 = bitcast i8* %t7628 to i8**
  %t7630 = load i8*, i8** %t7629, align 8
  store i8* %t7630, i8** %t7626, align 8
  store i8* %t7616, i8** %t7629, align 8
  %t7636 = getelementptr inbounds i8, i8* %stackTop.77, i64 156
  %t7637 = bitcast i8* %t7636 to i32*
  %t7638 = load i32, i32* %t7637, align 4
  %t7968 = getelementptr inbounds i8, i8* %stackTop.77, i64 72
  %t7969 = bitcast i8* %t7968 to i32*
  %t7970 = load i32, i32* %t7969, align 4
  %t7971.not = icmp slt i32 %t7638, %t7970
  br i1 %t7971.not, label %L_1406, label %L_1387

L_1421:                                           ; preds = %L_1420, %L_1420.preheader
  %t7641 = load i8*, i8** %t14203, align 8
  %t7643.not = icmp ult i8* %t7641, %frontier.853554
  br i1 %t7643.not, label %L_1422, label %L_1424

L_1420:                                           ; preds = %L_1425
  %t7592 = add nsw i32 %TW32_0.183545, -1
  %t7647 = icmp slt i32 %TW32_0.183545, 2
  br i1 %t7647, label %L_1421, label %L_1425

L_1418:                                           ; preds = %L_1417
  %cond46 = icmp eq i8* %TP_1.8.le, inttoptr (i64 1 to i8*)
  br i1 %cond46, label %L_1427, label %L_1420.preheader

L_1420.preheader:                                 ; preds = %L_1418
  %t76473544 = icmp eq i32 %t7663, 0
  br i1 %t76473544, label %L_1421, label %L_1425

L_1417:                                           ; preds = %L_1416, %L_1414.thread, %L_1416.preheader
  %TP_1.8.in.in.lcssa = phi i8* [ %t7688, %L_1416.preheader ], [ %t76884561, %L_1414.thread ], [ %TP_0.353540, %L_1416 ]
  %TP_1.8.in.le = bitcast i8* %TP_1.8.in.in.lcssa to i8**
  %TP_1.8.le = load i8*, i8** %TP_1.8.in.le, align 8
  %t7661 = getelementptr inbounds i8, i8* %stackTop.843552, i64 140
  %t7662 = bitcast i8* %t7661 to i32*
  %t7663 = load i32, i32* %t7662, align 4
  %trunc2046 = icmp sgt i32 %t7663, -1
  br i1 %trunc2046, label %L_1418, label %L_1428

L_1416:                                           ; preds = %L_1429
  %t7558 = add nsw i32 %TW32_0.193539, -1
  %t7668 = icmp slt i32 %TW32_0.193539, 2
  br i1 %t7668, label %L_1417, label %L_1429

L_1414:                                           ; preds = %L_1413
  %t7686 = getelementptr inbounds i8, i8* %stackTop.843552, i64 80
  %t7687 = bitcast i8* %t7686 to i8**
  %t7688 = load i8*, i8** %t7687, align 8
  %cond44 = icmp eq i8* %t7688, inttoptr (i64 1 to i8*)
  br i1 %cond44, label %L_1431, label %L_1416.preheader

L_1414.thread:                                    ; preds = %L_1413.preheader
  %t76864559 = getelementptr inbounds i8, i8* %stackTop.843552, i64 80
  %t76874560 = bitcast i8* %t76864559 to i8**
  %t76884561 = load i8*, i8** %t76874560, align 8
  %cond444562 = icmp eq i8* %t76884561, inttoptr (i64 1 to i8*)
  br i1 %cond444562, label %L_1431, label %L_1417

L_1416.preheader:                                 ; preds = %L_1414
  br i1 %t76913532, label %L_1417, label %L_1429

L_1413:                                           ; preds = %L_1432
  %t7531 = add nsw i32 %TW32_1.83533, -1
  %t7691 = icmp slt i32 %TW32_1.83533, 2
  br i1 %t7691, label %L_1414, label %L_1432

L_1411:                                           ; preds = %L_1410
  %cond42 = icmp eq i8* %TP_1.9.le, inttoptr (i64 1 to i8*)
  br i1 %cond42, label %L_1434, label %L_1413.preheader

L_1413.preheader:                                 ; preds = %L_1411
  %t76913532 = icmp eq i32 %TW32_0.203551, 0
  br i1 %t76913532, label %L_1414.thread, label %L_1432

L_1410:                                           ; preds = %L_1409, %L_1409.preheader
  %TP_1.9.in.in.lcssa = phi i8* [ %t7730, %L_1409.preheader ], [ %TP_0.373528, %L_1409 ]
  %TP_1.9.in.le = bitcast i8* %TP_1.9.in.in.lcssa to i8**
  %TP_1.9.le = load i8*, i8** %TP_1.9.in.le, align 8
  %trunc2043 = icmp sgt i32 %TW32_0.203551, -1
  br i1 %trunc2043, label %L_1411, label %L_1435

L_1409:                                           ; preds = %L_1436
  %t7497 = add nsw i32 %TW32_1.93527, -1
  %t7706 = icmp slt i32 %TW32_1.93527, 2
  br i1 %t7706, label %L_1410, label %L_1436

L_1407:                                           ; preds = %L_1406
  %t7728 = getelementptr inbounds i8, i8* %stackTop.843552, i64 64
  %t7729 = bitcast i8* %t7728 to i8**
  %t7730 = load i8*, i8** %t7729, align 8
  %cond40 = icmp eq i8* %t7730, inttoptr (i64 1 to i8*)
  br i1 %cond40, label %L_1438, label %L_1409.preheader

L_1409.preheader:                                 ; preds = %L_1407
  %t77063526 = icmp eq i32 %t7740, 0
  br i1 %t77063526, label %L_1410, label %L_1436

L_1406:                                           ; preds = %L_1386, %L_1424
  %frontier.853554 = phi i8* [ %t7623, %L_1424 ], [ %frontier.873569, %L_1386 ]
  %stackTop.843552 = phi i8* [ %stackTop.77, %L_1424 ], [ %stackTop.863568, %L_1386 ]
  %TW32_0.203551 = phi i32 [ %t7638, %L_1424 ], [ 0, %L_1386 ]
  %t7733 = add nsw i32 %TW32_0.203551, 1
  %t7735 = getelementptr inbounds i8, i8* %stackTop.843552, i64 156
  %t7736 = bitcast i8* %t7735 to i32*
  store i32 %t7733, i32* %t7736, align 4
  %t7738 = getelementptr inbounds i8, i8* %stackTop.843552, i64 76
  %t7739 = bitcast i8* %t7738 to i32*
  %t7740 = load i32, i32* %t7739, align 4
  %trunc2041 = icmp sgt i32 %t7740, -1
  br i1 %trunc2041, label %L_1407, label %L_1439

L_1390:                                           ; preds = %L_1389
  %t7747 = getelementptr inbounds i8, i8* %stackTop.83, i64 184
  %t7748 = bitcast i8* %t7747 to i64*
  store i64 113, i64* %t7748, align 4
  %t7750 = getelementptr inbounds i8, i8* %stackTop.83, i64 192
  store i8* %frontier.84, i8** %t15197, align 8
  store i8* %t7750, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t7760 = load i8*, i8** %t15197, align 8
  %t7763 = load i8*, i8** %t15200, align 8
  %t7745 = getelementptr inbounds i8, i8* %t7763, i64 -192
  br label %L_1392

L_1396:                                           ; preds = %L_1395
  %t7767 = getelementptr inbounds i8, i8* %stackTop.81, i64 184
  %t7768 = bitcast i8* %t7767 to i64*
  store i64 113, i64* %t7768, align 4
  %t7770 = getelementptr inbounds i8, i8* %stackTop.81, i64 192
  store i8* %frontier.82, i8** %t15197, align 8
  store i8* %t7770, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t7780 = load i8*, i8** %t15197, align 8
  %t7783 = load i8*, i8** %t15200, align 8
  %t7765 = getelementptr inbounds i8, i8* %t7783, i64 -192
  br label %L_1398

L_1403:                                           ; preds = %L_1402
  %t7787 = getelementptr inbounds i8, i8* %stackTop.79, i64 168
  %t7788 = bitcast i8* %t7787 to i64*
  store i64 112, i64* %t7788, align 4
  %t7790 = getelementptr inbounds i8, i8* %stackTop.79, i64 176
  store i8* %frontier.80, i8** %t15197, align 8
  store i8* %t7790, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t7800 = load i8*, i8** %t15197, align 8
  %t7803 = load i8*, i8** %t15200, align 8
  %t7785 = getelementptr inbounds i8, i8* %t7803, i64 -176
  br label %L_1405

L_1405:                                           ; preds = %L_1402, %L_1403
  %stackTop.78 = phi i8* [ %t7785, %L_1403 ], [ %stackTop.79, %L_1402 ]
  %frontier.79 = phi i8* [ %t7800, %L_1403 ], [ %frontier.80, %L_1402 ]
  %t7805 = getelementptr inbounds i8, i8* %frontier.79, i64 8
  %t7810 = bitcast i8* %frontier.79 to i64*
  store i64 85, i64* %t7810, align 4
  %t7812 = getelementptr inbounds i8, i8* %frontier.79, i64 16
  %t7815 = bitcast i8* %t7805 to i8**
  %t7817 = getelementptr inbounds i8, i8* %stackTop.78, i64 144
  %t7818 = bitcast i8* %t7817 to i8**
  %t7819 = load i8*, i8** %t7818, align 8
  store i8* %t7819, i8** %t7815, align 8
  store i8* %t7805, i8** %t7818, align 8
  %t7825 = getelementptr inbounds i8, i8* %stackTop.78, i64 140
  %t7826 = bitcast i8* %t7825 to i32*
  %t7828 = getelementptr inbounds i8, i8* %stackTop.78, i64 152
  %t7829 = bitcast i8* %t7828 to i32*
  %t7830 = load i32, i32* %t7829, align 4
  store i32 %t7830, i32* %t7826, align 4
  %t8064 = getelementptr inbounds i8, i8* %stackTop.78, i64 72
  %t8065 = bitcast i8* %t8064 to i32*
  %t8066 = load i32, i32* %t8065, align 4
  %t8067.not = icmp slt i32 %t7830, %t8066
  br i1 %t8067.not, label %L_1386, label %L_1378

L_1402:                                           ; preds = %L_1401, %L_1386, %loop_75.preheader, %L_1387
  %stackTop.79 = phi i8* [ %stackTop.77, %L_1387 ], [ %stackTop.80, %loop_75.preheader ], [ %stackTop.863568, %L_1386 ], [ %stackTop.80, %L_1401 ]
  %frontier.80 = phi i8* [ %t7623, %L_1387 ], [ %t7872, %loop_75.preheader ], [ %frontier.873569, %L_1386 ], [ %t7872, %L_1401 ]
  %t7833 = load i8*, i8** %t14203, align 8
  %t7835.not = icmp ult i8* %t7833, %frontier.80
  br i1 %t7835.not, label %L_1403, label %L_1405

L_1401:                                           ; preds = %loop_75.preheader, %L_1401
  %TP_0.383560 = phi i8* [ %TP_0.38, %L_1401 ], [ %TP_0.383558, %loop_75.preheader ]
  %t7840 = bitcast i8* %TP_0.383560 to i8**
  %TP_0.38 = load i8*, i8** %t7840, align 8
  %cond51 = icmp eq i8* %TP_0.38, inttoptr (i64 1 to i8*)
  br i1 %cond51, label %L_1402, label %L_1401

L_1399:                                           ; preds = %L_1398
  %t7854 = bitcast i8* %t7883 to i8**
  %t7855 = load i8*, i8** %t7854, align 8
  store i8* %t7855, i8** %t7882, align 8
  store i8* %t7865, i8** %t7878, align 8
  br label %L_1395

L_1398:                                           ; preds = %L_1395, %L_1396
  %stackTop.80 = phi i8* [ %t7765, %L_1396 ], [ %stackTop.81, %L_1395 ]
  %frontier.81 = phi i8* [ %t7780, %L_1396 ], [ %frontier.82, %L_1395 ]
  %t7865 = getelementptr inbounds i8, i8* %frontier.81, i64 8
  %t7870 = bitcast i8* %frontier.81 to i64*
  store i64 85, i64* %t7870, align 4
  %t7872 = getelementptr inbounds i8, i8* %frontier.81, i64 16
  %t7875 = bitcast i8* %t7865 to i8**
  %t7877 = getelementptr inbounds i8, i8* %stackTop.80, i64 176
  %t7878 = bitcast i8* %t7877 to i8**
  %t7879 = load i8*, i8** %t7878, align 8
  store i8* %t7879, i8** %t7875, align 8
  %t7881 = getelementptr inbounds i8, i8* %stackTop.80, i64 168
  %t7882 = bitcast i8* %t7881 to i8**
  %t7883 = load i8*, i8** %t7882, align 8
  %cond50 = icmp eq i8* %t7883, inttoptr (i64 1 to i8*)
  br i1 %cond50, label %loop_75.preheader, label %L_1399

loop_75.preheader:                                ; preds = %L_1398
  %t7878.le = bitcast i8* %t7877 to i8**
  %TP_0.383558 = load i8*, i8** %t7878.le, align 8
  %cond513559 = icmp eq i8* %TP_0.383558, inttoptr (i64 1 to i8*)
  br i1 %cond513559, label %L_1402, label %L_1401

L_1395:                                           ; preds = %L_1394, %L_1399
  %stackTop.81 = phi i8* [ %stackTop.82, %L_1394 ], [ %stackTop.80, %L_1399 ]
  %frontier.82 = phi i8* [ %t7926, %L_1394 ], [ %t7872, %L_1399 ]
  %t7887 = load i8*, i8** %t14203, align 8
  %t7889.not = icmp ult i8* %t7887, %frontier.82
  br i1 %t7889.not, label %L_1396, label %L_1398

L_1394:                                           ; preds = %L_1392
  %t7932.le = bitcast i8* %t7931 to i8**
  %t7936.le = bitcast i8* %t7935 to i8**
  %t7898 = load i8*, i8** %t7932.le, align 8
  store i8* %t7898, i8** %t7936.le, align 8
  store i8* inttoptr (i64 1 to i8*), i8** %t7932.le, align 8
  br label %L_1395

L_1393:                                           ; preds = %L_1392
  %t7908 = bitcast i8* %t7937 to i8**
  %t7909 = load i8*, i8** %t7908, align 8
  store i8* %t7909, i8** %t7936, align 8
  store i8* %t7919, i8** %t7932, align 8
  br label %L_1389

L_1392:                                           ; preds = %L_1389, %L_1390
  %stackTop.82 = phi i8* [ %t7745, %L_1390 ], [ %stackTop.83, %L_1389 ]
  %frontier.83 = phi i8* [ %t7760, %L_1390 ], [ %frontier.84, %L_1389 ]
  %t7919 = getelementptr inbounds i8, i8* %frontier.83, i64 8
  %t7924 = bitcast i8* %frontier.83 to i64*
  store i64 85, i64* %t7924, align 4
  %t7926 = getelementptr inbounds i8, i8* %frontier.83, i64 16
  %t7929 = bitcast i8* %t7919 to i8**
  %t7931 = getelementptr inbounds i8, i8* %stackTop.82, i64 176
  %t7932 = bitcast i8* %t7931 to i8**
  %t7933 = load i8*, i8** %t7932, align 8
  store i8* %t7933, i8** %t7929, align 8
  %t7935 = getelementptr inbounds i8, i8* %stackTop.82, i64 168
  %t7936 = bitcast i8* %t7935 to i8**
  %t7937 = load i8*, i8** %t7936, align 8
  %cond49 = icmp eq i8* %t7937, inttoptr (i64 1 to i8*)
  br i1 %cond49, label %L_1394, label %L_1393

L_1389:                                           ; preds = %L_1388, %L_1393
  %stackTop.83 = phi i8* [ %stackTop.77, %L_1388 ], [ %stackTop.82, %L_1393 ]
  %frontier.84 = phi i8* [ %t7623, %L_1388 ], [ %t7926, %L_1393 ]
  %t7941 = load i8*, i8** %t14203, align 8
  %t7943.not = icmp ult i8* %t7941, %frontier.84
  br i1 %t7943.not, label %L_1390, label %L_1392

L_1388:                                           ; preds = %L_1387
  %t7629.le = bitcast i8* %t7628 to i8**
  %t7951 = bitcast i8* %t7616 to i8**
  %t7952 = load i8*, i8** %t7951, align 8
  store i8* %t7952, i8** %t7629.le, align 8
  %t7958 = getelementptr inbounds i8, i8* %stackTop.77, i64 176
  %t7959 = bitcast i8* %t7958 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t7959, align 8
  br label %L_1389

L_1387:                                           ; preds = %L_1424
  %cond48 = icmp eq i8* %t7616, inttoptr (i64 1 to i8*)
  br i1 %cond48, label %L_1402, label %L_1388

L_1386:                                           ; preds = %L_1377, %L_1405
  %t79703549 = phi i32 [ %t8066, %L_1405 ], [ %t80663566, %L_1377 ]
  %t80623570 = phi i32 [ %t7830, %L_1405 ], [ 0, %L_1377 ]
  %frontier.873569 = phi i8* [ %t7812, %L_1405 ], [ %frontier.1013581, %L_1377 ]
  %stackTop.863568 = phi i8* [ %stackTop.78, %L_1405 ], [ %stackTop.1013580, %L_1377 ]
  %t7978 = add nsw i32 %t80623570, 1
  %t7980 = getelementptr inbounds i8, i8* %stackTop.863568, i64 152
  %t7981 = bitcast i8* %t7980 to i32*
  store i32 %t7978, i32* %t7981, align 4
  %t7983 = getelementptr inbounds i8, i8* %stackTop.863568, i64 168
  %t7984 = bitcast i8* %t7983 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t7984, align 8
  %t7971.not3550 = icmp sgt i32 %t79703549, 0
  br i1 %t7971.not3550, label %L_1406, label %L_1402

L_1383:                                           ; preds = %L_1382
  %t7989 = getelementptr inbounds i8, i8* %stackTop.86.lcssa4574, i64 168
  %t7990 = bitcast i8* %t7989 to i64*
  store i64 111, i64* %t7990, align 4
  %t7992 = getelementptr inbounds i8, i8* %stackTop.86.lcssa4574, i64 176
  store i8* %frontier.87.lcssa4575, i8** %t15197, align 8
  store i8* %t7992, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t8002 = load i8*, i8** %t15197, align 8
  %t8005 = load i8*, i8** %t15200, align 8
  %t7987 = getelementptr inbounds i8, i8* %t8005, i64 -176
  br label %L_1385

L_1385:                                           ; preds = %L_1382, %L_1383
  %stackTop.85 = phi i8* [ %t7987, %L_1383 ], [ %stackTop.86.lcssa4574, %L_1382 ]
  %frontier.86 = phi i8* [ %t8002, %L_1383 ], [ %frontier.87.lcssa4575, %L_1382 ]
  %t8007 = getelementptr inbounds i8, i8* %frontier.86, i64 8
  %t8012 = bitcast i8* %frontier.86 to i64*
  store i64 101, i64* %t8012, align 4
  %t8014 = getelementptr inbounds i8, i8* %frontier.86, i64 16
  %t8017 = bitcast i8* %t8007 to i8**
  %t8019 = getelementptr inbounds i8, i8* %stackTop.85, i64 128
  %t8020 = bitcast i8* %t8019 to i8**
  %t8021 = load i8*, i8** %t8020, align 8
  store i8* %t8021, i8** %t8017, align 8
  store i8* %t8007, i8** %t8020, align 8
  %t8027 = getelementptr inbounds i8, i8* %stackTop.85, i64 76
  %t8028 = bitcast i8* %t8027 to i32*
  %t8030 = getelementptr inbounds i8, i8* %stackTop.85, i64 136
  %t8031 = bitcast i8* %t8030 to i32*
  %t8032 = load i32, i32* %t8031, align 4
  store i32 %t8032, i32* %t8028, align 4
  %t10032 = getelementptr inbounds i8, i8* %stackTop.85, i64 72
  %t10033 = bitcast i8* %t10032 to i32*
  %t10034 = load i32, i32* %t10033, align 4
  %t10035.not = icmp slt i32 %t8032, %t10034
  br i1 %t10035.not, label %L_1377, label %L_1201

L_1382:                                           ; preds = %L_1380, %L_1377, %L_1378
  %frontier.87.lcssa4575 = phi i8* [ %t7812, %L_1378 ], [ %frontier.1013581, %L_1377 ], [ %t7812, %L_1380 ]
  %stackTop.86.lcssa4574 = phi i8* [ %stackTop.78, %L_1378 ], [ %stackTop.1013580, %L_1377 ], [ %stackTop.78, %L_1380 ]
  %t8035 = load i8*, i8** %t14203, align 8
  %t8037.not = icmp ult i8* %t8035, %frontier.87.lcssa4575
  br i1 %t8037.not, label %L_1383, label %L_1385

L_1380:                                           ; preds = %L_1378, %L_1380
  %TP_0.39.in.in = phi i8* [ %TP_0.39, %L_1380 ], [ %t7805, %L_1378 ]
  %TP_0.39.in = bitcast i8* %TP_0.39.in.in to i8**
  %TP_0.39 = load i8*, i8** %TP_0.39.in, align 8
  %cond53 = icmp eq i8* %TP_0.39, inttoptr (i64 1 to i8*)
  br i1 %cond53, label %L_1382, label %L_1380

L_1378:                                           ; preds = %L_1405
  %cond52 = icmp eq i8* %t7805, inttoptr (i64 1 to i8*)
  br i1 %cond52, label %L_1382, label %L_1380

L_1377:                                           ; preds = %L_1200, %L_1385
  %t80663566 = phi i32 [ %t10034, %L_1385 ], [ %t100343578, %L_1200 ]
  %t100303582 = phi i32 [ %t8032, %L_1385 ], [ 0, %L_1200 ]
  %frontier.1013581 = phi i8* [ %t8014, %L_1385 ], [ %frontier.0, %L_1200 ]
  %stackTop.1013580 = phi i8* [ %stackTop.85, %L_1385 ], [ %t10070, %L_1200 ]
  %t8074 = add nsw i32 %t100303582, 1
  %t8076 = getelementptr inbounds i8, i8* %stackTop.1013580, i64 136
  %t8077 = bitcast i8* %t8076 to i32*
  store i32 %t8074, i32* %t8077, align 4
  %t8079 = getelementptr inbounds i8, i8* %stackTop.1013580, i64 144
  %t8080 = bitcast i8* %t8079 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t8080, align 8
  %t8083 = getelementptr inbounds i8, i8* %stackTop.1013580, i64 140
  %t8084 = bitcast i8* %t8083 to i32*
  store i32 0, i32* %t8084, align 4
  %t8067.not3567 = icmp sgt i32 %t80663566, 0
  br i1 %t8067.not3567, label %L_1386, label %L_1382

L_1773:                                           ; preds = %L_1200, %L_1201
  %frontier.101.lcssa4607 = phi i8* [ %t8014, %L_1201 ], [ %frontier.0, %L_1200 ]
  %stackTop.101.lcssa4605 = phi i8* [ %stackTop.85, %L_1201 ], [ %t10070, %L_1200 ]
  %t8086 = getelementptr inbounds i8, i8* %stackTop.101.lcssa4605, i64 64
  %t8087 = bitcast i8* %t8086 to i8**
  %t8089 = getelementptr inbounds i8, i8* %stackTop.101.lcssa4605, i64 88
  %t8090 = bitcast i8* %t8089 to i8**
  %t8091 = load i8*, i8** %t8090, align 8
  store i8* %t8091, i8** %t8087, align 8
  br label %L_1205

L_1374:                                           ; preds = %L_1759
  %t8106 = tail call i32 @IntInf_compare(i8* %gcState, i8* %t9968, i8* %t9962)
  %t8093 = icmp slt i32 %t8106, 1
  %t9919.pre.pre = load i8*, i8** %t9967, align 8
  br i1 %t8093, label %L_1209, label %L_1374.L_1373_crit_edge

L_1374.L_1373_crit_edge:                          ; preds = %L_1374
  %.pre4495 = ptrtoint i8* %t9919.pre.pre to i64
  br label %L_1373

L_1373:                                           ; preds = %L_1374.L_1373_crit_edge, %L_1207
  %.pre4494.pre-phi = phi i64 [ %.pre4495, %L_1374.L_1373_crit_edge ], [ %t9969, %L_1207 ]
  %t8113 = phi i8* [ %t9919.pre.pre, %L_1374.L_1373_crit_edge ], [ %t9968, %L_1207 ]
  %t8108 = getelementptr inbounds i8, i8* %stackTop.0, i64 -104
  %t8109 = bitcast i8* %t8108 to i8**
  store i8* %t8113, i8** %t8109, align 8
  br label %L_1210

L_1372:                                           ; preds = %L_1370
  %t8115 = getelementptr inbounds i8, i8* %stackTop.0, i64 -152
  %t8116 = bitcast i8* %t8115 to i8**
  %t8118 = getelementptr inbounds i8, i8* %stackTop.0, i64 -16
  %t8119 = bitcast i8* %t8118 to i8**
  %t8120 = load i8*, i8** %t8119, align 8
  store i8* %t8120, i8** %t8116, align 8
  %t8122 = getelementptr inbounds i8, i8* %stackTop.0, i64 -120
  br label %L_981

L_1371:                                           ; preds = %L_1370
  %t8135 = add nsw i64 %t8133, -1
  br label %L_1354

L_1370:                                           ; preds = %L_1352
  %t8131 = getelementptr inbounds i8, i8* %t9924, i64 -16
  %t8132 = bitcast i8* %t8131 to i64*
  %t8133 = load i64, i64* %t8132, align 4
  %t8138.not = icmp eq i64 %t8133, -9223372036854775808
  br i1 %t8138.not, label %L_1372, label %L_1371

L_1369:                                           ; preds = %L_1367
  %t8142 = getelementptr inbounds i8, i8* %stackTop.0, i64 -152
  %t8143 = bitcast i8* %t8142 to i8**
  %t8145 = getelementptr inbounds i8, i8* %stackTop.0, i64 -16
  %t8146 = bitcast i8* %t8145 to i8**
  %t8147 = load i8*, i8** %t8146, align 8
  store i8* %t8147, i8** %t8143, align 8
  %t8149 = getelementptr inbounds i8, i8* %stackTop.0, i64 -120
  br label %L_981

L_1368:                                           ; preds = %L_1367
  %t8162 = add nsw i64 %t8160, -1
  br label %L_1356

L_1367:                                           ; preds = %L_1354
  %t8158 = getelementptr inbounds i8, i8* %t9919, i64 -16
  %t8159 = bitcast i8* %t8158 to i64*
  %t8160 = load i64, i64* %t8159, align 4
  %t8165.not = icmp eq i64 %t8160, -9223372036854775808
  br i1 %t8165.not, label %L_1369, label %L_1368

L_1361:                                           ; preds = %L_1356
  %t8176 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t8177 = bitcast i8* %t8176 to i64*
  store i64 110, i64* %t8177, align 4
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %stackTop.0, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 %t8235, i32 0)
  %t8189 = load i8*, i8** %t15197, align 8
  %t8192 = load i8*, i8** %t15200, align 8
  %t8170 = getelementptr inbounds i8, i8* %t8192, i64 -176
  %t8199.phi.trans.insert = getelementptr inbounds i8, i8* %t8192, i64 -104
  %t8200.phi.trans.insert = bitcast i8* %t8199.phi.trans.insert to i8**
  %t8201.pre = load i8*, i8** %t8200.phi.trans.insert, align 8
  br label %L_1363

L_1363:                                           ; preds = %L_1356, %L_1361
  %t8201 = phi i8* [ %t8201.pre, %L_1361 ], [ %t9924, %L_1356 ]
  %stackTop.87 = phi i8* [ %t8170, %L_1361 ], [ %t9958, %L_1356 ]
  %frontier.88 = phi i8* [ %t8189, %L_1361 ], [ %frontier.0, %L_1356 ]
  %t8199 = getelementptr inbounds i8, i8* %stackTop.87, i64 72
  %t8200 = bitcast i8* %t8199 to i8**
  %t8203 = getelementptr inbounds i8, i8* %stackTop.87, i64 96
  %t8204 = bitcast i8* %t8203 to i8**
  %t8205 = load i8*, i8** %t8204, align 8
  %t8207 = getelementptr inbounds i8, i8* %stackTop.87, i64 80
  %t8208 = bitcast i8* %t8207 to i64*
  %t8209 = load i64, i64* %t8208, align 4
  store i8* %frontier.88, i8** %t15197, align 8
  %t8213 = tail call i8* @IntInf_sub(i8* nonnull %gcState, i8* %t8201, i8* %t8205, i64 %t8209)
  %t8216 = load i8*, i8** %t15197, align 8
  store i8* %t8213, i8** %t8200, align 8
  br label %x_6

L_1356:                                           ; preds = %L_1354, %L_1368
  %TW64_1.16 = phi i64 [ %t8162, %L_1368 ], [ 1, %L_1354 ]
  %t8241.not = icmp slt i64 %TW64_0.15, %TW64_1.16
  %spec.select2372 = select i1 %t8241.not, i64 %TW64_1.16, i64 %TW64_0.15
  %t8233 = shl i64 %spec.select2372, 3
  %t8235 = add i64 %t8233, 47
  %t8237 = getelementptr inbounds i8, i8* %stackTop.0, i64 -96
  %t8238 = bitcast i8* %t8237 to i64*
  store i64 %t8235, i64* %t8238, align 4
  %t8219 = load i8*, i8** %t8218, align 8
  %t8221 = ptrtoint i8* %t8219 to i64
  %t8222 = ptrtoint i8* %frontier.0 to i64
  %t8223 = sub i64 %t8221, %t8222
  %t8229.not = icmp ult i64 %t8223, %t8235
  br i1 %t8229.not, label %L_1361, label %L_1363

L_1354:                                           ; preds = %L_1352, %L_1371
  %TW64_0.15 = phi i64 [ %t8135, %L_1371 ], [ 1, %L_1352 ]
  %.not2065.not = icmp eq i64 %t9926, 0
  br i1 %.not2065.not, label %L_1367, label %L_1356

L_1352:                                           ; preds = %L_1210, %L_1211
  %84 = and i64 %t9925.pre-phi, 1
  %.not2062.not = icmp eq i64 %84, 0
  br i1 %.not2062.not, label %L_1370, label %L_1354

L_1351:                                           ; preds = %L_1349
  %t8265 = getelementptr inbounds i8, i8* %stackTop.100, i64 24
  %t8266 = bitcast i8* %t8265 to i8**
  %t8268 = getelementptr inbounds i8, i8* %stackTop.100, i64 160
  %t8269 = bitcast i8* %t8268 to i8**
  %t8270 = load i8*, i8** %t8269, align 8
  store i8* %t8270, i8** %t8266, align 8
  %t8272 = getelementptr inbounds i8, i8* %stackTop.100, i64 56
  br label %L_981

L_1350:                                           ; preds = %L_1349
  %t8285 = add nsw i64 %t8283, -1
  br label %L_1331

L_1349:                                           ; preds = %L_1329
  %t8281 = getelementptr inbounds i8, i8* %t9867, i64 -16
  %t8282 = bitcast i8* %t8281 to i64*
  %t8283 = load i64, i64* %t8282, align 4
  %t8288.not = icmp eq i64 %t8283, -9223372036854775808
  br i1 %t8288.not, label %L_1351, label %L_1350

L_1348:                                           ; preds = %L_1346
  %t8292 = getelementptr inbounds i8, i8* %stackTop.100, i64 24
  %t8293 = bitcast i8* %t8292 to i8**
  %t8295 = getelementptr inbounds i8, i8* %stackTop.100, i64 160
  %t8296 = bitcast i8* %t8295 to i8**
  %t8297 = load i8*, i8** %t8296, align 8
  store i8* %t8297, i8** %t8293, align 8
  %t8299 = getelementptr inbounds i8, i8* %stackTop.100, i64 56
  br label %L_981

L_1347:                                           ; preds = %L_1346
  %t8312 = add nsw i64 %t8310, -1
  br label %L_1333

L_1346:                                           ; preds = %L_1331
  %t8308 = getelementptr inbounds i8, i8* %t9850, i64 -16
  %t8309 = bitcast i8* %t8308 to i64*
  %t8310 = load i64, i64* %t8309, align 4
  %t8315.not = icmp eq i64 %t8310, -9223372036854775808
  br i1 %t8315.not, label %L_1348, label %L_1347

L_1344:                                           ; preds = %L_1334
  %t8320 = getelementptr inbounds i8, i8* %stackTop.100, i64 24
  %t8321 = bitcast i8* %t8320 to i8**
  %t8323 = getelementptr inbounds i8, i8* %stackTop.100, i64 160
  %t8324 = bitcast i8* %t8323 to i8**
  %t8325 = load i8*, i8** %t8324, align 8
  store i8* %t8325, i8** %t8321, align 8
  %t8327 = getelementptr inbounds i8, i8* %stackTop.100, i64 56
  br label %L_981

L_1343:                                           ; preds = %L_1335
  %t8332 = getelementptr inbounds i8, i8* %stackTop.100, i64 24
  %t8333 = bitcast i8* %t8332 to i8**
  %t8335 = getelementptr inbounds i8, i8* %stackTop.100, i64 160
  %t8336 = bitcast i8* %t8335 to i8**
  %t8337 = load i8*, i8** %t8336, align 8
  store i8* %t8337, i8** %t8333, align 8
  %t8339 = getelementptr inbounds i8, i8* %stackTop.100, i64 56
  br label %L_981

L_1338:                                           ; preds = %L_1341
  %t8352 = getelementptr inbounds i8, i8* %stackTop.100, i64 168
  %t8353 = bitcast i8* %t8352 to i64*
  store i64 109, i64* %t8353, align 4
  %t8355 = getelementptr inbounds i8, i8* %stackTop.100, i64 176
  store i8* %frontier.100, i8** %t15197, align 8
  store i8* %t8355, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 %t8416, i32 0)
  %t8365 = load i8*, i8** %t15197, align 8
  %t8368 = load i8*, i8** %t15200, align 8
  %t8346 = getelementptr inbounds i8, i8* %t8368, i64 -176
  %t8371.phi.trans.insert = getelementptr inbounds i8, i8* %t8368, i64 -104
  %t8372.phi.trans.insert = bitcast i8* %t8371.phi.trans.insert to i8**
  %t8373.pre = load i8*, i8** %t8372.phi.trans.insert, align 8
  %t8375.phi.trans.insert = getelementptr inbounds i8, i8* %t8368, i64 -96
  %t8376.phi.trans.insert = bitcast i8* %t8375.phi.trans.insert to i8**
  %t8377.pre = load i8*, i8** %t8376.phi.trans.insert, align 8
  %t8379.phi.trans.insert = getelementptr inbounds i8, i8* %t8368, i64 -128
  %t8380.phi.trans.insert = bitcast i8* %t8379.phi.trans.insert to i64*
  %t8381.pre = load i64, i64* %t8380.phi.trans.insert, align 4
  br label %L_1340

L_1340:                                           ; preds = %L_1341, %L_1338
  %t8381 = phi i64 [ %t8381.pre, %L_1338 ], [ %t8416, %L_1341 ]
  %t8377 = phi i8* [ %t8377.pre, %L_1338 ], [ %t9850, %L_1341 ]
  %t8373 = phi i8* [ %t8373.pre, %L_1338 ], [ %t9867, %L_1341 ]
  %stackTop.88 = phi i8* [ %t8346, %L_1338 ], [ %stackTop.100, %L_1341 ]
  %frontier.89 = phi i8* [ %t8365, %L_1338 ], [ %frontier.100, %L_1341 ]
  store i8* %frontier.89, i8** %t15197, align 8
  %t8385 = tail call i8* @IntInf_quot(i8* nonnull %gcState, i8* %t8373, i8* %t8377, i64 %t8381)
  %t8388 = load i8*, i8** %t15197, align 8
  br label %L_1218

L_1341:                                           ; preds = %L_1335
  %t8422 = sub i64 %TW64_1.17, %TW64_2.3
  %reass.add2643 = add i64 %t8422, 2
  %reass.mul = mul i64 %reass.add2643, %t9858
  %t8413 = add i64 %t9858, 31
  %t8416 = add i64 %t8413, %reass.mul
  %t8419 = bitcast i8* %t9835 to i64*
  store i64 %t8416, i64* %t8419, align 4
  %t8391 = load i8*, i8** %t8218, align 8
  %t8393 = ptrtoint i8* %t8391 to i64
  %t8394 = ptrtoint i8* %frontier.100 to i64
  %t8395 = sub i64 %t8393, %t8394
  %t8401.not = icmp ult i64 %t8395, %t8416
  br i1 %t8401.not, label %L_1338, label %L_1340

L_1335:                                           ; preds = %L_1334
  %t8425 = tail call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %TW64_1.17, i64 %TW64_2.3)
  %t8426 = extractvalue { i64, i1 } %t8425, 1
  br i1 %t8426, label %L_1343, label %L_1341

L_1334:                                           ; preds = %L_1333
  %t8436.not = icmp eq i8* %t9850, inttoptr (i64 1 to i8*)
  br i1 %t8436.not, label %L_1344, label %L_1335

L_1333:                                           ; preds = %L_1331, %L_1347
  %TW64_2.3 = phi i64 [ %t8312, %L_1347 ], [ 1, %L_1331 ]
  %t8441.not = icmp slt i64 %TW64_1.17, %TW64_2.3
  br i1 %t8441.not, label %L_1218, label %L_1334

L_1331:                                           ; preds = %L_1329, %L_1350
  %TW64_1.17 = phi i64 [ %t8285, %L_1350 ], [ 1, %L_1329 ]
  %.not2075.not = icmp eq i64 %t9869, 0
  br i1 %.not2075.not, label %L_1346, label %L_1333

L_1329:                                           ; preds = %x_6
  %85 = and i64 %t9868, 1
  %.not2072.not = icmp eq i64 %85, 0
  br i1 %.not2072.not, label %L_1349, label %L_1331

L_1326:                                           ; preds = %L_1215
  %t8470 = getelementptr inbounds i8, i8* %stackTop.100, i64 24
  %t8471 = bitcast i8* %t8470 to i8**
  %t8473 = getelementptr inbounds i8, i8* %stackTop.100, i64 160
  %t8474 = bitcast i8* %t8473 to i8**
  %t8475 = load i8*, i8** %t8474, align 8
  store i8* %t8475, i8** %t8471, align 8
  %t8477 = getelementptr inbounds i8, i8* %stackTop.100, i64 56
  br label %L_981

L_1321:                                           ; preds = %L_1319
  %t8507 = load double, double* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 9232) to double*), align 8
  %t8509 = fcmp uge double %t8507, %t9754
  %spec.select2385 = select i1 %t8509, i8* getelementptr (i8, i8* @staticHeapI, i64 3432), i8* getelementptr (i8, i8* @staticHeapI, i64 3400)
  br label %L_1253

L_1319:                                           ; preds = %L_1760
  %t8515 = and i64 %t9756, 4503599627370495
  %t8517.not = icmp eq i64 %t8515, 0
  br i1 %t8517.not, label %L_1321, label %L_1253

L_1314:                                           ; preds = %L_1311
  %t8529 = getelementptr inbounds i8, i8* %stackTop.0, i64 -16
  %t8530 = bitcast i8* %t8529 to i8**
  %t8531 = load i8*, i8** %t8530, align 8
  store i8* %t8531, i8** %t9689, align 8
  %t8533 = getelementptr inbounds i8, i8* %stackTop.0, i64 -120
  br label %L_981

L_1312:                                           ; preds = %L_1311
  %t8542 = add i32 %t8560, -1
  store i32 %t8542, i32* %t13969, align 4
  %t8550 = getelementptr inbounds i8, i8* %stackTop.0, i64 -16
  %t8551 = bitcast i8* %t8550 to i8**
  %t8552 = load i8*, i8** %t8551, align 8
  store i8* %t8552, i8** %t9689, align 8
  %t8554 = getelementptr inbounds i8, i8* %stackTop.0, i64 -120
  br label %L_981

L_1311:                                           ; preds = %L_1761
  %t8560 = load i32, i32* %t13969, align 4
  %t8562.not = icmp eq i32 %t8560, 0
  br i1 %t8562.not, label %L_1314, label %L_1312

L_1310:                                           ; preds = %L_1310.lr.ph, %L_1310
  %TW64_0.173700 = phi i64 [ 0, %L_1310.lr.ph ], [ %t8592, %L_1310 ]
  %86 = shl i64 %TW64_0.173700, 32
  %t8573 = ashr exact i64 %86, 32
  %t8577 = getelementptr inbounds i8, i8* %t9608, i64 %t8573
  %t8580 = load i8, i8* %t8577, align 1
  %t8584 = load i8*, i8** %t8583, align 8
  %t8587 = getelementptr inbounds i8, i8* %t8584, i64 %TW64_0.173700
  store i8 %t8580, i8* %t8587, align 1
  %t8592 = add nuw nsw i64 %TW64_0.173700, 1
  %t9601 = load i64, i64* %t9600, align 4
  %t9602.not = icmp slt i64 %t8592, %t9601
  br i1 %t9602.not, label %L_1310, label %L_1236

L_1309:                                           ; preds = %L_1236
  %t8598 = getelementptr inbounds i8, i8* %stackTop.98, i64 160
  %t8599 = bitcast i8* %t8598 to i8**
  %t8600 = load i8*, i8** %t8599, align 8
  store i8* %t8600, i8** %t9585, align 8
  %t8602 = getelementptr inbounds i8, i8* %stackTop.98, i64 56
  br label %L_981

L_1308:                                           ; preds = %L_1289
  %t8612 = getelementptr inbounds i8, i8* %stackTop.98, i64 160
  %t8613 = bitcast i8* %t8612 to i8**
  %t8614 = load i8*, i8** %t8613, align 8
  store i8* %t8614, i8** %t9585, align 8
  %t8616 = getelementptr inbounds i8, i8* %stackTop.98, i64 56
  br label %L_981

L_1307:                                           ; preds = %L_1290
  %t8624 = getelementptr inbounds i8, i8* %stackTop.98, i64 160
  %t8625 = bitcast i8* %t8624 to i8**
  %t8626 = load i8*, i8** %t8625, align 8
  store i8* %t8626, i8** %t9585, align 8
  %t8628 = getelementptr inbounds i8, i8* %stackTop.98, i64 56
  br label %L_981

L_1305:                                           ; preds = %L_1770
  %t8645 = bitcast i8* %t8932 to i8**
  %t8647 = getelementptr inbounds i8, i8* %t8982, i64 -16
  %t8648 = bitcast i8* %t8647 to i8**
  %t8649 = load i8*, i8** %t8648, align 8
  store i8* %t8649, i8** %t8645, align 8
  %t8651 = getelementptr inbounds i8, i8* %t8982, i64 -120
  br label %L_981

L_1304:                                           ; preds = %L_1294
  %t8657 = bitcast i8* %t8932 to i8**
  %t8659 = getelementptr inbounds i8, i8* %t8982, i64 -16
  %t8660 = bitcast i8* %t8659 to i8**
  %t8661 = load i8*, i8** %t8660, align 8
  store i8* %t8661, i8** %t8657, align 8
  %t8663 = getelementptr inbounds i8, i8* %t8982, i64 -120
  br label %L_981

L_1303:                                           ; preds = %L_1295
  %t8669 = bitcast i8* %t8932 to i8**
  %t8671 = getelementptr inbounds i8, i8* %t8982, i64 -16
  %t8672 = bitcast i8* %t8671 to i8**
  %t8673 = load i8*, i8** %t8672, align 8
  store i8* %t8673, i8** %t8669, align 8
  %t8675 = getelementptr inbounds i8, i8* %t8982, i64 -120
  br label %L_981

L_1299:                                           ; preds = %L_1771
  %t8693 = getelementptr inbounds i8, i8* %stackTop.90, i64 168
  %t8694 = bitcast i8* %t8693 to i64*
  store i64 108, i64* %t8694, align 4
  %t8696 = getelementptr inbounds i8, i8* %stackTop.90, i64 176
  store i8* %frontier.91, i8** %t15197, align 8
  store i8* %t8696, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t8706 = load i8*, i8** %t15197, align 8
  %t8709 = load i8*, i8** %t15200, align 8
  %t8691 = getelementptr inbounds i8, i8* %t8709, i64 -176
  %t8717.phi.trans.insert = getelementptr inbounds i8, i8* %t8709, i64 -152
  %t8718.phi.trans.insert = bitcast i8* %t8717.phi.trans.insert to i8**
  %t8719.pre = load i8*, i8** %t8718.phi.trans.insert, align 8
  br label %L_1301

L_1772:                                           ; preds = %doSwitchNextBlock
  %t8714 = bitcast i8* %stackTop.0 to i8**
  %t8715 = load i8*, i8** %t8714, align 8
  br label %L_1253

L_1301:                                           ; preds = %L_1771, %L_1299
  %t8718.pre-phi = phi i8** [ %t8840, %L_1771 ], [ %t8718.phi.trans.insert, %L_1299 ]
  %t8719 = phi i8* [ %TP_0.40, %L_1771 ], [ %t8719.pre, %L_1299 ]
  %stackTop.89 = phi i8* [ %stackTop.90, %L_1771 ], [ %t8691, %L_1299 ]
  %frontier.90 = phi i8* [ %frontier.91, %L_1771 ], [ %t8706, %L_1299 ]
  %t8720 = getelementptr inbounds i8, i8* %t8719, i64 -8
  %t8721 = bitcast i8* %t8720 to i64*
  store i64 11, i64* %t8721, align 4
  %t8723 = getelementptr inbounds i8, i8* %frontier.90, i64 8
  %t8728 = bitcast i8* %frontier.90 to i64*
  store i64 123, i64* %t8728, align 4
  %t8733 = bitcast i8* %t8723 to i8**
  %t8737 = load i8*, i8** %t8718.pre-phi, align 8
  store i8* %t8737, i8** %t8733, align 8
  %t8740 = getelementptr inbounds i8, i8* %frontier.90, i64 16
  %t8741 = bitcast i8* %t8740 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t8741, align 8
  %t8744 = getelementptr inbounds i8, i8* %frontier.90, i64 32
  %t8748 = getelementptr inbounds i8, i8* %frontier.90, i64 24
  %t8749 = bitcast i8* %t8748 to i64*
  store i64 123, i64* %t8749, align 4
  %t8754 = bitcast i8* %t8744 to i8**
  %t8756 = getelementptr inbounds i8, i8* %stackTop.89, i64 72
  %t8757 = bitcast i8* %t8756 to i8**
  %t8758 = load i8*, i8** %t8757, align 8
  store i8* %t8758, i8** %t8754, align 8
  %t8761 = getelementptr inbounds i8, i8* %frontier.90, i64 40
  %t8762 = bitcast i8* %t8761 to i8**
  store i8* %t8723, i8** %t8762, align 8
  %t8765 = getelementptr inbounds i8, i8* %frontier.90, i64 56
  %t8769 = getelementptr inbounds i8, i8* %frontier.90, i64 48
  %t8770 = bitcast i8* %t8769 to i64*
  store i64 123, i64* %t8770, align 4
  %t8775 = bitcast i8* %t8765 to i8**
  %t8777 = getelementptr inbounds i8, i8* %stackTop.89, i64 48
  %t8778 = bitcast i8* %t8777 to i8**
  %t8779 = load i8*, i8** %t8778, align 8
  store i8* %t8779, i8** %t8775, align 8
  %t8782 = getelementptr inbounds i8, i8* %frontier.90, i64 64
  %t8783 = bitcast i8* %t8782 to i8**
  store i8* %t8744, i8** %t8783, align 8
  %t8786 = getelementptr inbounds i8, i8* %frontier.90, i64 80
  %t8790 = getelementptr inbounds i8, i8* %frontier.90, i64 72
  %t8791 = bitcast i8* %t8790 to i64*
  store i64 123, i64* %t8791, align 4
  %t8796 = bitcast i8* %t8786 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 3528), i8** %t8796, align 8
  %t8800 = getelementptr inbounds i8, i8* %frontier.90, i64 88
  %t8801 = bitcast i8* %t8800 to i8**
  store i8* %t8765, i8** %t8801, align 8
  %t8804 = getelementptr inbounds i8, i8* %frontier.90, i64 104
  %t8808 = getelementptr inbounds i8, i8* %frontier.90, i64 96
  %t8809 = bitcast i8* %t8808 to i64*
  store i64 123, i64* %t8809, align 4
  %t8811 = getelementptr inbounds i8, i8* %frontier.90, i64 120
  %t8814 = bitcast i8* %t8804 to i8**
  %t8816 = getelementptr inbounds i8, i8* %stackTop.89, i64 40
  %t8817 = bitcast i8* %t8816 to i8**
  %t8818 = load i8*, i8** %t8817, align 8
  store i8* %t8818, i8** %t8814, align 8
  %t8820 = getelementptr inbounds i8, i8* %frontier.90, i64 112
  %t8821 = bitcast i8* %t8820 to i8**
  store i8* %t8786, i8** %t8821, align 8
  %t8824 = getelementptr inbounds i8, i8* %stackTop.89, i64 176
  %t8825 = bitcast i8* %t8824 to i8**
  store i8* %t8804, i8** %t8825, align 8
  %t8828 = getelementptr inbounds i8, i8* %stackTop.89, i64 168
  %t8829 = bitcast i8* %t8828 to i64*
  store i64 59, i64* %t8829, align 4
  store i8* %t8811, i8** %t15197, align 8
  store i8* %t8824, i8** %t15200, align 8
  br label %common.ret

L_1771:                                           ; preds = %L_1302.preheader, %L_join_8
  %t8840 = bitcast i8* %t8860 to i8**
  store i8* %TP_0.40, i8** %t8840, align 8
  store i8* %t8866, i8** %t8865, align 8
  %t8848 = load i8*, i8** %t14203, align 8
  %t8850.not = icmp ult i8* %t8848, %frontier.91
  br i1 %t8850.not, label %L_1299, label %L_1301

L_join_8:                                         ; preds = %L_1296, %L_nonZeroLen_13
  %TP_0.40 = phi i8* [ %t8885, %L_nonZeroLen_13 ], [ getelementptr (i8, i8* @staticHeapM, i64 64), %L_1296 ]
  %stackTop.90 = phi i8* [ %t8868, %L_nonZeroLen_13 ], [ %t8959, %L_1296 ]
  %frontier.91 = phi i8* [ %t8888, %L_nonZeroLen_13 ], [ %t8979, %L_1296 ]
  %t8860 = getelementptr inbounds i8, i8* %stackTop.90, i64 24
  %t8861 = bitcast i8* %t8860 to i64*
  %t8862 = load i64, i64* %t8861, align 4
  %t8864 = getelementptr inbounds i8, i8* %stackTop.90, i64 48
  %t8865 = bitcast i8* %t8864 to i8**
  %t8866 = load i8*, i8** %t8865, align 8
  %t8855.not3704 = icmp sgt i64 %t8862, 0
  br i1 %t8855.not3704, label %L_1302.preheader, label %L_1771

L_1302.preheader:                                 ; preds = %L_join_8
  call void @llvm.memset.p0i8.i64(i8* align 1 %TP_0.40, i8 48, i64 %t8862, i1 false)
  br label %L_1771

L_nonZeroLen_13:                                  ; preds = %L_1296
  %t8875 = getelementptr inbounds i8, i8* %t8982, i64 -8
  %t8876 = bitcast i8* %t8875 to i64*
  store i64 107, i64* %t8876, align 4
  store i8* %t8979, i8** %t15197, align 8
  store i8* %t8982, i8** %t15200, align 8
  %t8885 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 %t8900, i64 21)
  %t8888 = load i8*, i8** %t15197, align 8
  %t8891 = load i8*, i8** %t15200, align 8
  %t8868 = getelementptr inbounds i8, i8* %t8891, i64 -176
  br label %L_join_8

L_1296:                                           ; preds = %L_1295
  %t8896.not = icmp eq i32 %t8913, 0
  br i1 %t8896.not, label %L_join_8, label %L_nonZeroLen_13

L_1295:                                           ; preds = %L_1294
  %t8913 = add i32 %t8935, %t8957
  %t8900 = sext i32 %t8913 to i64
  %t8903 = bitcast i8* %t8932 to i64*
  store i64 %t8900, i64* %t8903, align 4
  %trunc2107 = icmp sgt i32 %t8913, -1
  br i1 %trunc2107, label %L_1296, label %L_1303

L_1294:                                           ; preds = %L_1770
  %t8916 = tail call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %t8935, i32 %t8957)
  %t8917 = extractvalue { i32, i1 } %t8916, 1
  br i1 %t8917, label %L_1304, label %L_1295

L_1770:                                           ; preds = %L_1306.preheader, %L_1291
  %t8922 = bitcast i8* %t8951 to i8**
  store i8* %t8976, i8** %t8922, align 8
  %t8929 = getelementptr inbounds i8, i8* %t8976, i64 -8
  %t8930 = bitcast i8* %t8929 to i64*
  store i64 11, i64* %t8930, align 4
  %t8932 = getelementptr inbounds i8, i8* %t8982, i64 -152
  %t8933 = bitcast i8* %t8932 to i32*
  %t8934 = load i32, i32* %t8933, align 4
  %t8935 = sub i32 10, %t8934
  %t8940 = tail call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 10, i32 %t8934)
  %t8941 = extractvalue { i32, i1 } %t8940, 1
  br i1 %t8941, label %L_1305, label %L_1294

L_1291:                                           ; preds = %L_1290
  %t8966 = getelementptr inbounds i8, i8* %stackTop.98, i64 168
  %t8967 = bitcast i8* %t8966 to i64*
  store i64 102, i64* %t8967, align 4
  %t8969 = getelementptr inbounds i8, i8* %stackTop.98, i64 176
  store i8* %frontier.98, i8** %t15197, align 8
  store i8* %t8969, i8** %t15200, align 8
  %t8976 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 %t8991, i64 21)
  %t8979 = load i8*, i8** %t15197, align 8
  %t8982 = load i8*, i8** %t15200, align 8
  %t8959 = getelementptr inbounds i8, i8* %t8982, i64 -176
  %t8951 = getelementptr inbounds i8, i8* %t8982, i64 -128
  %t8952 = bitcast i8* %t8951 to i64*
  %t8953 = load i64, i64* %t8952, align 4
  %t8955 = getelementptr inbounds i8, i8* %t8982, i64 -112
  %t8956 = bitcast i8* %t8955 to i32*
  %t8957 = load i32, i32* %t8956, align 4
  %t8946.not3702 = icmp sgt i64 %t8953, 0
  br i1 %t8946.not3702, label %L_1306.preheader, label %L_1770

L_1306.preheader:                                 ; preds = %L_1291
  call void @llvm.memset.p0i8.i64(i8* align 1 %t8976, i8 48, i64 %t8953, i1 false)
  br label %L_1770

L_1290:                                           ; preds = %L_1289
  %t9006 = sub i32 0, %t9528
  %t8991 = sext i32 %t9006 to i64
  store i64 %t8991, i64* %t9600, align 4
  %trunc2102 = icmp sgt i32 %t9006, -1
  br i1 %trunc2102, label %L_1291, label %L_1307

L_1289:                                           ; preds = %L_1240
  %t9011 = tail call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 0, i32 %t9528)
  %t9012 = extractvalue { i32, i1 } %t9011, 1
  br i1 %t9012, label %L_1308, label %L_1290

L_1288:                                           ; preds = %L_1241
  %t9016 = getelementptr inbounds i8, i8* %stackTop.98, i64 80
  br label %L_1274

L_1286:                                           ; preds = %L_1284
  %t9037 = load i8*, i8** %t9036, align 8
  %t9041 = getelementptr inbounds i8, i8* %t9037, i64 %t9058
  %t9044 = load i8, i8* %t9041, align 1
  br label %L_1283

L_1285:                                           ; preds = %L_1284
  %t9047 = bitcast i8* %t9081 to i8**
  %t9049 = getelementptr inbounds i8, i8* %t9495, i64 -16
  %t9050 = bitcast i8* %t9049 to i8**
  %t9051 = load i8*, i8** %t9050, align 8
  store i8* %t9051, i8** %t9047, align 8
  %t9053 = getelementptr inbounds i8, i8* %t9495, i64 -120
  br label %L_981

L_1284:                                           ; preds = %L_1281
  %t9058 = sext i32 %t9078 to i64
  %t9063 = load i64, i64* %t9062, align 4
  %t9064.not = icmp ugt i64 %t9063, %t9058
  br i1 %t9064.not, label %L_1286, label %L_1285

L_1283:                                           ; preds = %L_1281, %L_1286
  %TW8_0.0 = phi i8 [ %t9044, %L_1286 ], [ 48, %L_1281 ]
  %t9070 = getelementptr inbounds i8, i8* %t9489, i64 %TW64_1.203707
  store i8 %TW8_0.0, i8* %t9070, align 1
  %t9075 = add nuw nsw i64 %TW64_1.203707, 1
  %t9460.not = icmp slt i64 %t9075, %t9466
  br i1 %t9460.not, label %L_1281, label %L_1762

L_1281:                                           ; preds = %L_1281.lr.ph, %L_1283
  %TW64_1.203707 = phi i64 [ 0, %L_1281.lr.ph ], [ %t9075, %L_1283 ]
  %t9078 = trunc i64 %TW64_1.203707 to i32
  %t9083 = load i32, i32* %t9082, align 4
  %t9084.not = icmp sgt i32 %t9083, %t9078
  br i1 %t9084.not, label %L_1284, label %L_1283

L_1280:                                           ; preds = %L_1276.8, %L_1276.7, %L_1276.6, %L_1276.5, %L_1276.4, %L_1276.3, %L_1276.2, %L_1276.1, %L_1276
  %t9089 = bitcast i8* %t9144 to i8**
  %t9091 = getelementptr inbounds i8, i8* %t9448, i64 -16
  %t9092 = bitcast i8* %t9091 to i8**
  %t9093 = load i8*, i8** %t9092, align 8
  store i8* %t9093, i8** %t9089, align 8
  %t9095 = getelementptr inbounds i8, i8* %t9448, i64 -120
  br label %L_981

L_1276.thread:                                    ; preds = %L_1277
  %t9102 = load i8*, i8** %t9101, align 8
  %t9106 = getelementptr inbounds i8, i8* %t9102, i64 %t9123
  %t9109 = load i8, i8* %t9106, align 1
  store i8 %t9109, i8* %t9442, align 1
  br label %L_1274.1

L_1278:                                           ; preds = %L_1277.9, %L_1277.8, %L_1277.7, %L_1277.6, %L_1277.5, %L_1277.4, %L_1277.3, %L_1277.2, %L_1277.1, %L_1277
  %t9112 = bitcast i8* %t9144 to i8**
  %t9114 = getelementptr inbounds i8, i8* %t9448, i64 -16
  %t9115 = bitcast i8* %t9114 to i8**
  %t9116 = load i8*, i8** %t9115, align 8
  store i8* %t9116, i8** %t9112, align 8
  %t9118 = getelementptr inbounds i8, i8* %t9448, i64 -120
  br label %L_981

L_1277:                                           ; preds = %L_1274
  %t9123 = sext i32 %t9430 to i64
  %t9128 = load i64, i64* %t9127, align 4
  %t9129.not = icmp ugt i64 %t9128, %t9123
  br i1 %t9129.not, label %L_1276.thread, label %L_1278

L_1276:                                           ; preds = %L_1274
  store i8 48, i8* %t9442, align 1
  %t9158.1 = icmp eq i32 %t9430, 2147483647
  br i1 %t9158.1, label %L_1280, label %L_1274.1

L_1274.1:                                         ; preds = %L_1276.thread, %L_1276
  %t9154.14579 = add i32 %t9430, 1
  %t9146.1 = load i32, i32* %t9145, align 4
  %t9147.not.1 = icmp slt i32 %t9154.14579, %t9146.1
  br i1 %t9147.not.1, label %L_1277.1, label %L_1276.1

L_1277.1:                                         ; preds = %L_1274.1
  %t9123.1 = sext i32 %t9154.14579 to i64
  %t9128.1 = load i64, i64* %t9127, align 4
  %t9129.not.1 = icmp ugt i64 %t9128.1, %t9123.1
  br i1 %t9129.not.1, label %L_1279.1, label %L_1278

L_1279.1:                                         ; preds = %L_1277.1
  %t9102.1 = load i8*, i8** %t9101, align 8
  %t9106.1 = getelementptr inbounds i8, i8* %t9102.1, i64 %t9123.1
  %t9109.1 = load i8, i8* %t9106.1, align 1
  br label %L_1276.1

L_1276.1:                                         ; preds = %L_1279.1, %L_1274.1
  %TW8_0.1.1 = phi i8 [ %t9109.1, %L_1279.1 ], [ 48, %L_1274.1 ]
  %t9135.1 = getelementptr inbounds i8, i8* %t9442, i64 1
  store i8 %TW8_0.1.1, i8* %t9135.1, align 1
  %t9154.2 = add i32 %t9430, 2
  %t9158.2 = icmp sgt i32 %t9430, 2147483645
  br i1 %t9158.2, label %L_1280, label %L_1274.2

L_1274.2:                                         ; preds = %L_1276.1
  %t9146.2 = load i32, i32* %t9145, align 4
  %t9147.not.2 = icmp slt i32 %t9154.2, %t9146.2
  br i1 %t9147.not.2, label %L_1277.2, label %L_1276.2

L_1277.2:                                         ; preds = %L_1274.2
  %t9123.2 = sext i32 %t9154.2 to i64
  %t9128.2 = load i64, i64* %t9127, align 4
  %t9129.not.2 = icmp ugt i64 %t9128.2, %t9123.2
  br i1 %t9129.not.2, label %L_1276.2.thread, label %L_1278

L_1276.2.thread:                                  ; preds = %L_1277.2
  %t9102.2 = load i8*, i8** %t9101, align 8
  %t9106.2 = getelementptr inbounds i8, i8* %t9102.2, i64 %t9123.2
  %t9109.2 = load i8, i8* %t9106.2, align 1
  %t9135.24581 = getelementptr inbounds i8, i8* %t9442, i64 2
  store i8 %t9109.2, i8* %t9135.24581, align 1
  br label %L_1274.3

L_1276.2:                                         ; preds = %L_1274.2
  %t9135.2 = getelementptr inbounds i8, i8* %t9442, i64 2
  store i8 48, i8* %t9135.2, align 1
  %t9158.3 = icmp sgt i32 %t9430, 2147483644
  br i1 %t9158.3, label %L_1280, label %L_1274.3

L_1274.3:                                         ; preds = %L_1276.2.thread, %L_1276.2
  %t9154.34584 = add i32 %t9430, 3
  %t9146.3 = load i32, i32* %t9145, align 4
  %t9147.not.3 = icmp slt i32 %t9154.34584, %t9146.3
  br i1 %t9147.not.3, label %L_1277.3, label %L_1276.3

L_1277.3:                                         ; preds = %L_1274.3
  %t9123.3 = sext i32 %t9154.34584 to i64
  %t9128.3 = load i64, i64* %t9127, align 4
  %t9129.not.3 = icmp ugt i64 %t9128.3, %t9123.3
  br i1 %t9129.not.3, label %L_1279.3, label %L_1278

L_1279.3:                                         ; preds = %L_1277.3
  %t9102.3 = load i8*, i8** %t9101, align 8
  %t9106.3 = getelementptr inbounds i8, i8* %t9102.3, i64 %t9123.3
  %t9109.3 = load i8, i8* %t9106.3, align 1
  br label %L_1276.3

L_1276.3:                                         ; preds = %L_1279.3, %L_1274.3
  %TW8_0.1.3 = phi i8 [ %t9109.3, %L_1279.3 ], [ 48, %L_1274.3 ]
  %t9135.3 = getelementptr inbounds i8, i8* %t9442, i64 3
  store i8 %TW8_0.1.3, i8* %t9135.3, align 1
  %t9154.4 = add i32 %t9430, 4
  %t9158.4 = icmp sgt i32 %t9430, 2147483643
  br i1 %t9158.4, label %L_1280, label %L_1274.4

L_1274.4:                                         ; preds = %L_1276.3
  %t9146.4 = load i32, i32* %t9145, align 4
  %t9147.not.4 = icmp slt i32 %t9154.4, %t9146.4
  br i1 %t9147.not.4, label %L_1277.4, label %L_1276.4

L_1277.4:                                         ; preds = %L_1274.4
  %t9123.4 = sext i32 %t9154.4 to i64
  %t9128.4 = load i64, i64* %t9127, align 4
  %t9129.not.4 = icmp ugt i64 %t9128.4, %t9123.4
  br i1 %t9129.not.4, label %L_1276.4.thread, label %L_1278

L_1276.4.thread:                                  ; preds = %L_1277.4
  %t9102.4 = load i8*, i8** %t9101, align 8
  %t9106.4 = getelementptr inbounds i8, i8* %t9102.4, i64 %t9123.4
  %t9109.4 = load i8, i8* %t9106.4, align 1
  %t9135.44586 = getelementptr inbounds i8, i8* %t9442, i64 4
  store i8 %t9109.4, i8* %t9135.44586, align 1
  br label %L_1274.5

L_1276.4:                                         ; preds = %L_1274.4
  %t9135.4 = getelementptr inbounds i8, i8* %t9442, i64 4
  store i8 48, i8* %t9135.4, align 1
  %t9158.5 = icmp sgt i32 %t9430, 2147483642
  br i1 %t9158.5, label %L_1280, label %L_1274.5

L_1274.5:                                         ; preds = %L_1276.4.thread, %L_1276.4
  %t9154.54589 = add i32 %t9430, 5
  %t9146.5 = load i32, i32* %t9145, align 4
  %t9147.not.5 = icmp slt i32 %t9154.54589, %t9146.5
  br i1 %t9147.not.5, label %L_1277.5, label %L_1276.5

L_1277.5:                                         ; preds = %L_1274.5
  %t9123.5 = sext i32 %t9154.54589 to i64
  %t9128.5 = load i64, i64* %t9127, align 4
  %t9129.not.5 = icmp ugt i64 %t9128.5, %t9123.5
  br i1 %t9129.not.5, label %L_1279.5, label %L_1278

L_1279.5:                                         ; preds = %L_1277.5
  %t9102.5 = load i8*, i8** %t9101, align 8
  %t9106.5 = getelementptr inbounds i8, i8* %t9102.5, i64 %t9123.5
  %t9109.5 = load i8, i8* %t9106.5, align 1
  br label %L_1276.5

L_1276.5:                                         ; preds = %L_1279.5, %L_1274.5
  %TW8_0.1.5 = phi i8 [ %t9109.5, %L_1279.5 ], [ 48, %L_1274.5 ]
  %t9135.5 = getelementptr inbounds i8, i8* %t9442, i64 5
  store i8 %TW8_0.1.5, i8* %t9135.5, align 1
  %t9154.6 = add i32 %t9430, 6
  %t9158.6 = icmp sgt i32 %t9430, 2147483641
  br i1 %t9158.6, label %L_1280, label %L_1274.6

L_1274.6:                                         ; preds = %L_1276.5
  %t9146.6 = load i32, i32* %t9145, align 4
  %t9147.not.6 = icmp slt i32 %t9154.6, %t9146.6
  br i1 %t9147.not.6, label %L_1277.6, label %L_1276.6

L_1277.6:                                         ; preds = %L_1274.6
  %t9123.6 = sext i32 %t9154.6 to i64
  %t9128.6 = load i64, i64* %t9127, align 4
  %t9129.not.6 = icmp ugt i64 %t9128.6, %t9123.6
  br i1 %t9129.not.6, label %L_1276.6.thread, label %L_1278

L_1276.6.thread:                                  ; preds = %L_1277.6
  %t9102.6 = load i8*, i8** %t9101, align 8
  %t9106.6 = getelementptr inbounds i8, i8* %t9102.6, i64 %t9123.6
  %t9109.6 = load i8, i8* %t9106.6, align 1
  %t9135.64591 = getelementptr inbounds i8, i8* %t9442, i64 6
  store i8 %t9109.6, i8* %t9135.64591, align 1
  br label %L_1274.7

L_1276.6:                                         ; preds = %L_1274.6
  %t9135.6 = getelementptr inbounds i8, i8* %t9442, i64 6
  store i8 48, i8* %t9135.6, align 1
  %t9158.7 = icmp sgt i32 %t9430, 2147483640
  br i1 %t9158.7, label %L_1280, label %L_1274.7

L_1274.7:                                         ; preds = %L_1276.6.thread, %L_1276.6
  %t9154.74594 = add i32 %t9430, 7
  %t9146.7 = load i32, i32* %t9145, align 4
  %t9147.not.7 = icmp slt i32 %t9154.74594, %t9146.7
  br i1 %t9147.not.7, label %L_1277.7, label %L_1276.7

L_1277.7:                                         ; preds = %L_1274.7
  %t9123.7 = sext i32 %t9154.74594 to i64
  %t9128.7 = load i64, i64* %t9127, align 4
  %t9129.not.7 = icmp ugt i64 %t9128.7, %t9123.7
  br i1 %t9129.not.7, label %L_1279.7, label %L_1278

L_1279.7:                                         ; preds = %L_1277.7
  %t9102.7 = load i8*, i8** %t9101, align 8
  %t9106.7 = getelementptr inbounds i8, i8* %t9102.7, i64 %t9123.7
  %t9109.7 = load i8, i8* %t9106.7, align 1
  br label %L_1276.7

L_1276.7:                                         ; preds = %L_1279.7, %L_1274.7
  %TW8_0.1.7 = phi i8 [ %t9109.7, %L_1279.7 ], [ 48, %L_1274.7 ]
  %t9135.7 = getelementptr inbounds i8, i8* %t9442, i64 7
  store i8 %TW8_0.1.7, i8* %t9135.7, align 1
  %t9154.8 = add i32 %t9430, 8
  %t9158.8 = icmp sgt i32 %t9430, 2147483639
  br i1 %t9158.8, label %L_1280, label %L_1274.8

L_1274.8:                                         ; preds = %L_1276.7
  %t9146.8 = load i32, i32* %t9145, align 4
  %t9147.not.8 = icmp slt i32 %t9154.8, %t9146.8
  br i1 %t9147.not.8, label %L_1277.8, label %L_1276.8

L_1277.8:                                         ; preds = %L_1274.8
  %t9123.8 = sext i32 %t9154.8 to i64
  %t9128.8 = load i64, i64* %t9127, align 4
  %t9129.not.8 = icmp ugt i64 %t9128.8, %t9123.8
  br i1 %t9129.not.8, label %L_1276.8.thread, label %L_1278

L_1276.8.thread:                                  ; preds = %L_1277.8
  %t9102.8 = load i8*, i8** %t9101, align 8
  %t9106.8 = getelementptr inbounds i8, i8* %t9102.8, i64 %t9123.8
  %t9109.8 = load i8, i8* %t9106.8, align 1
  %t9135.84596 = getelementptr inbounds i8, i8* %t9442, i64 8
  store i8 %t9109.8, i8* %t9135.84596, align 1
  br label %L_1274.9

L_1276.8:                                         ; preds = %L_1274.8
  %t9135.8 = getelementptr inbounds i8, i8* %t9442, i64 8
  store i8 48, i8* %t9135.8, align 1
  %t9158.9 = icmp sgt i32 %t9430, 2147483638
  br i1 %t9158.9, label %L_1280, label %L_1274.9

L_1274.9:                                         ; preds = %L_1276.8.thread, %L_1276.8
  %t9154.94599 = add i32 %t9430, 9
  %t9146.9 = load i32, i32* %t9145, align 4
  %t9147.not.9 = icmp slt i32 %t9154.94599, %t9146.9
  br i1 %t9147.not.9, label %L_1277.9, label %L_1276.9

L_1277.9:                                         ; preds = %L_1274.9
  %t9123.9 = sext i32 %t9154.94599 to i64
  %t9128.9 = load i64, i64* %t9127, align 4
  %t9129.not.9 = icmp ugt i64 %t9128.9, %t9123.9
  br i1 %t9129.not.9, label %L_1279.9, label %L_1278

L_1279.9:                                         ; preds = %L_1277.9
  %t9102.9 = load i8*, i8** %t9101, align 8
  %t9106.9 = getelementptr inbounds i8, i8* %t9102.9, i64 %t9123.9
  %t9109.9 = load i8, i8* %t9106.9, align 1
  br label %L_1276.9

L_1276.9:                                         ; preds = %L_1279.9, %L_1274.9
  %TW8_0.1.9 = phi i8 [ %t9109.9, %L_1279.9 ], [ 48, %L_1274.9 ]
  %t9135.9 = getelementptr inbounds i8, i8* %t9442, i64 9
  store i8 %TW8_0.1.9, i8* %t9135.9, align 1
  %t9410 = bitcast i8* %t9144 to i8**
  store i8* %t9442, i8** %t9410, align 8
  %t9414 = load i8*, i8** %t14203, align 8
  %t9416.not = icmp ult i8* %t9414, %t9445
  br i1 %t9416.not, label %L_1249, label %L_1251

L_1274:                                           ; preds = %L_1288, %L_1762
  %t9016.sink = phi i8* [ %t9016, %L_1288 ], [ %t9464, %L_1762 ]
  %.sink5215 = phi i8* [ getelementptr (i8, i8* @staticHeapI, i64 3560), %L_1288 ], [ %t9489, %L_1762 ]
  %stackTop.96 = phi i8* [ %stackTop.98, %L_1288 ], [ %t9472, %L_1762 ]
  %frontier.96 = phi i8* [ %frontier.98, %L_1288 ], [ %t9492, %L_1762 ]
  %t9017 = bitcast i8* %t9016.sink to i8**
  store i8* %.sink5215, i8** %t9017, align 8
  %t9432 = getelementptr inbounds i8, i8* %stackTop.96, i64 168
  %t9433 = bitcast i8* %t9432 to i64*
  store i64 103, i64* %t9433, align 4
  %t9435 = getelementptr inbounds i8, i8* %stackTop.96, i64 176
  store i8* %frontier.96, i8** %t15197, align 8
  store i8* %t9435, i8** %t15200, align 8
  %t9442 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 10, i64 21)
  %t9445 = load i8*, i8** %t15197, align 8
  %t9448 = load i8*, i8** %t15200, align 8
  %t9424 = getelementptr inbounds i8, i8* %t9448, i64 -176
  %t9428 = getelementptr inbounds i8, i8* %t9448, i64 -112
  %t9429 = bitcast i8* %t9428 to i32*
  %t9430 = load i32, i32* %t9429, align 4
  %t9144 = getelementptr inbounds i8, i8* %t9448, i64 -152
  %t9145 = bitcast i8* %t9144 to i32*
  %t9126 = getelementptr inbounds i8, i8* %t9448, i64 -128
  %t9127 = bitcast i8* %t9126 to i64*
  %t9100 = getelementptr inbounds i8, i8* %t9448, i64 -104
  %t9101 = bitcast i8* %t9100 to i8**
  %t9146 = load i32, i32* %t9145, align 4
  %t9147.not = icmp slt i32 %t9430, %t9146
  br i1 %t9147.not, label %L_1277, label %L_1276

L_1249:                                           ; preds = %L_1276.9
  %t9164 = getelementptr inbounds i8, i8* %t9448, i64 -8
  %t9165 = bitcast i8* %t9164 to i64*
  store i64 106, i64* %t9165, align 4
  store i8* %t9445, i8** %t15197, align 8
  store i8* %t9448, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t9177 = load i8*, i8** %t15197, align 8
  %t9180 = load i8*, i8** %t15200, align 8
  %t9162 = getelementptr inbounds i8, i8* %t9180, i64 -176
  br label %L_1251

L_1270:                                           ; preds = %L_1269
  %t9184 = getelementptr inbounds i8, i8* %stackTop.0, i64 152
  %t9185 = bitcast i8* %t9184 to i64*
  store i64 104, i64* %t9185, align 4
  %t9187 = getelementptr inbounds i8, i8* %stackTop.0, i64 160
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %t9187, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t9197 = load i8*, i8** %t15197, align 8
  %t9200 = load i8*, i8** %t15200, align 8
  %t9182 = getelementptr inbounds i8, i8* %t9200, i64 -176
  br label %L_1272

L_1272:                                           ; preds = %L_1269, %L_1270
  %stackTop.92 = phi i8* [ %t9182, %L_1270 ], [ %t9247, %L_1269 ]
  %frontier.93 = phi i8* [ %t9197, %L_1270 ], [ %frontier.0, %L_1269 ]
  %t9202 = getelementptr inbounds i8, i8* %stackTop.92, i64 24
  %t9203 = bitcast i8* %t9202 to i8**
  %87 = bitcast i8* %t9202 to i8***
  %t92042126 = load i8**, i8*** %87, align 8
  %t9207 = load i8*, i8** %t92042126, align 8
  %t9209 = getelementptr inbounds i8, i8* %frontier.93, i64 8
  %t9214 = bitcast i8* %frontier.93 to i64*
  store i64 127, i64* %t9214, align 4
  %t9216 = getelementptr inbounds i8, i8* %frontier.93, i64 32
  %t9219 = bitcast i8* %t9209 to i8**
  %t9221 = getelementptr inbounds i8, i8* %stackTop.92, i64 32
  %t9222 = bitcast i8* %t9221 to i8**
  %t9223 = load i8*, i8** %t9222, align 8
  store i8* %t9223, i8** %t9219, align 8
  %t9225 = getelementptr inbounds i8, i8* %frontier.93, i64 16
  %t9226 = bitcast i8* %t9225 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 3592), i8** %t9226, align 8
  %t9230 = getelementptr inbounds i8, i8* %frontier.93, i64 24
  %t9231 = bitcast i8* %t9230 to i8**
  store i8* %t9207, i8** %t9231, align 8
  %t9237 = getelementptr inbounds i8, i8* %stackTop.92, i64 160
  %t9238 = bitcast i8* %t9237 to i8**
  %t9239 = load i8*, i8** %t9238, align 8
  store i8* %t9239, i8** %t9203, align 8
  %t9241 = getelementptr inbounds i8, i8* %stackTop.92, i64 56
  br label %L_981

L_1269:                                           ; preds = %doSwitchNextBlock
  %t9247 = getelementptr inbounds i8, i8* %stackTop.0, i64 -16
  %t9249 = getelementptr inbounds i8, i8* %stackTop.0, i64 16
  %t9250 = bitcast i8* %t9249 to i8**
  %t9253 = bitcast i8* %stackTop.0 to i8**
  %t9254 = load i8*, i8** %t9253, align 8
  store i8* %t9254, i8** %t9250, align 8
  %t9257 = load i8*, i8** %t14203, align 8
  %t9259.not = icmp ult i8* %t9257, %frontier.0
  br i1 %t9259.not, label %L_1270, label %L_1272

L_1266.sink.split:                                ; preds = %L_227, %L_229, %L_257, %fromString_0, %L_250, %L_222, %L_182, %L_148, %L_193, %L_198, %L_131, %L_123, %L_127, %L_96.7, %L_96.6, %L_96.5, %L_96.4, %L_96.3, %L_96.2, %L_96.1, %L_96, %L_115, %L_85, %L_87, %L_134, %L_138, %L_108, %L_107, %L_153, %L_158, %L_155
  %.sink5216 = phi i8* [ %TP_0.1, %L_155 ], [ inttoptr (i64 6 to i8*), %L_158 ], [ inttoptr (i64 1 to i8*), %L_153 ], [ inttoptr (i64 6 to i8*), %L_107 ], [ inttoptr (i64 6 to i8*), %L_108 ], [ inttoptr (i64 1 to i8*), %L_138 ], [ inttoptr (i64 6 to i8*), %L_134 ], [ inttoptr (i64 1 to i8*), %L_87 ], [ inttoptr (i64 6 to i8*), %L_85 ], [ inttoptr (i64 6 to i8*), %L_115 ], [ inttoptr (i64 6 to i8*), %L_96 ], [ inttoptr (i64 6 to i8*), %L_96.1 ], [ inttoptr (i64 6 to i8*), %L_96.2 ], [ inttoptr (i64 6 to i8*), %L_96.3 ], [ inttoptr (i64 6 to i8*), %L_96.4 ], [ inttoptr (i64 6 to i8*), %L_96.5 ], [ inttoptr (i64 6 to i8*), %L_96.6 ], [ inttoptr (i64 6 to i8*), %L_96.7 ], [ inttoptr (i64 6 to i8*), %L_127 ], [ getelementptr (i8, i8* @staticHeapI, i64 9040), %L_123 ], [ getelementptr (i8, i8* @staticHeapI, i64 9040), %L_131 ], [ inttoptr (i64 1 to i8*), %L_198 ], [ inttoptr (i64 7 to i8*), %L_193 ], [ getelementptr (i8, i8* @staticHeapI, i64 8968), %L_148 ], [ getelementptr (i8, i8* @staticHeapI, i64 8968), %L_182 ], [ inttoptr (i64 2 to i8*), %L_222 ], [ inttoptr (i64 2 to i8*), %L_250 ], [ inttoptr (i64 1 to i8*), %fromString_0 ], [ inttoptr (i64 1 to i8*), %L_257 ], [ getelementptr (i8, i8* @staticHeapI, i64 9056), %L_229 ], [ getelementptr (i8, i8* @staticHeapI, i64 9072), %L_227 ]
  %frontier.94.ph = phi i8* [ %frontier.7, %L_155 ], [ %frontier.7, %L_158 ], [ %frontier.7, %L_153 ], [ %frontier.1, %L_107 ], [ %frontier.1, %L_108 ], [ %frontier.3, %L_138 ], [ %frontier.3, %L_134 ], [ %frontier.1, %L_87 ], [ %frontier.1, %L_85 ], [ %frontier.1, %L_115 ], [ %frontier.1, %L_96 ], [ %frontier.1, %L_96.1 ], [ %frontier.1, %L_96.2 ], [ %frontier.1, %L_96.3 ], [ %frontier.1, %L_96.4 ], [ %frontier.1, %L_96.5 ], [ %frontier.1, %L_96.6 ], [ %frontier.1, %L_96.7 ], [ %frontier.3, %L_127 ], [ %frontier.3, %L_123 ], [ %frontier.3, %L_131 ], [ %frontier.8, %L_198 ], [ %frontier.8, %L_193 ], [ %frontier.8, %L_148 ], [ %frontier.8, %L_182 ], [ %frontier.17, %L_222 ], [ %frontier.0, %L_250 ], [ %frontier.15, %fromString_0 ], [ %frontier.18, %L_257 ], [ %frontier.15, %L_229 ], [ %frontier.15, %L_227 ]
  %t1717 = load i8*, i8** %t14423, align 8
  %t1720 = load i64, i64* %t15180, align 4
  %t1721 = getelementptr inbounds i8, i8* %t1717, i64 %t1720
  %t1724 = bitcast i8* %t1721 to i8**
  store i8* %.sink5216, i8** %t1724, align 8
  %t1730 = load i8*, i8** %t14423, align 8
  %t1733 = load i64, i64* %t15180, align 4
  %t1734 = getelementptr inbounds i8, i8* %t1730, i64 %t1733
  br label %L_1266

L_1266:                                           ; preds = %doSwitchNextBlock, %L_1266.sink.split
  %stackTop.93 = phi i8* [ %t1734, %L_1266.sink.split ], [ %stackTop.0, %doSwitchNextBlock ]
  %frontier.94 = phi i8* [ %frontier.94.ph, %L_1266.sink.split ], [ %frontier.0, %doSwitchNextBlock ]
  %t9263 = getelementptr inbounds i8, i8* %stackTop.93, i64 -16
  %t9266 = bitcast i8* %stackTop.93 to i8**
  %t9267 = load i8*, i8** %t9266, align 8
  %t9269 = getelementptr inbounds i8, i8* %stackTop.93, i64 8
  %t9270 = bitcast i8* %t9269 to i8**
  %t9272 = getelementptr inbounds i8, i8* %stackTop.93, i64 144
  %t9273 = bitcast i8* %t9272 to i8**
  %t9274 = load i8*, i8** %t9273, align 8
  store i8* %t9274, i8** %t9270, align 8
  %t9277 = getelementptr inbounds i8, i8* %stackTop.93, i64 40
  br label %L_981

L_1253:                                           ; preds = %L_1321, %L_1319, %L_1764, %L_1772
  %TP_0.42 = phi i8* [ %t9307, %L_1764 ], [ %t8715, %L_1772 ], [ getelementptr (i8, i8* @staticHeapI, i64 3464), %L_1319 ], [ %spec.select2385, %L_1321 ]
  %t9282 = bitcast i8* %stackTop.0 to i8**
  store i8* %TP_0.42, i8** %t9282, align 8
  %t9285 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  %t9286 = bitcast i8* %t9285 to i8**
  %t9288 = getelementptr inbounds i8, i8* %stackTop.0, i64 -144
  %t9289 = bitcast i8* %t9288 to i8**
  %t9290 = load i8*, i8** %t9289, align 8
  store i8* %t9290, i8** %t9286, align 8
  %t9292 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t9293 = bitcast i8* %t9292 to i64*
  store i64 36, i64* %t9293, align 4
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %stackTop.0, i8** %t15200, align 8
  br label %common.ret

L_1764:                                           ; preds = %doSwitchNextBlock
  %t9306 = bitcast i8* %stackTop.0 to i8**
  %t9307 = load i8*, i8** %t9306, align 8
  br label %L_1253

L_1251:                                           ; preds = %L_1276.9, %L_1249
  %stackTop.95 = phi i8* [ %t9162, %L_1249 ], [ %t9424, %L_1276.9 ]
  %frontier.95 = phi i8* [ %t9177, %L_1249 ], [ %t9445, %L_1276.9 ]
  %t9309 = getelementptr inbounds i8, i8* %stackTop.95, i64 24
  %t9310 = bitcast i8* %t9309 to i8**
  %t9311 = load i8*, i8** %t9310, align 8
  %t9312 = getelementptr inbounds i8, i8* %t9311, i64 -8
  %t9313 = bitcast i8* %t9312 to i64*
  store i64 11, i64* %t9313, align 4
  %t9315 = getelementptr inbounds i8, i8* %frontier.95, i64 8
  %t9320 = bitcast i8* %frontier.95 to i64*
  store i64 123, i64* %t9320, align 4
  %t9325 = bitcast i8* %t9315 to i8**
  %t9329 = load i8*, i8** %t9310, align 8
  store i8* %t9329, i8** %t9325, align 8
  %t9332 = getelementptr inbounds i8, i8* %frontier.95, i64 16
  %t9333 = bitcast i8* %t9332 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t9333, align 8
  %t9336 = getelementptr inbounds i8, i8* %frontier.95, i64 32
  %t9340 = getelementptr inbounds i8, i8* %frontier.95, i64 24
  %t9341 = bitcast i8* %t9340 to i64*
  store i64 123, i64* %t9341, align 4
  %t9346 = bitcast i8* %t9336 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 3784), i8** %t9346, align 8
  %t9350 = getelementptr inbounds i8, i8* %frontier.95, i64 40
  %t9351 = bitcast i8* %t9350 to i8**
  store i8* %t9315, i8** %t9351, align 8
  %t9354 = getelementptr inbounds i8, i8* %frontier.95, i64 56
  %t9358 = getelementptr inbounds i8, i8* %frontier.95, i64 48
  %t9359 = bitcast i8* %t9358 to i64*
  store i64 123, i64* %t9359, align 4
  %t9364 = bitcast i8* %t9354 to i8**
  %t9366 = getelementptr inbounds i8, i8* %stackTop.95, i64 80
  %t9367 = bitcast i8* %t9366 to i8**
  %t9368 = load i8*, i8** %t9367, align 8
  store i8* %t9368, i8** %t9364, align 8
  %t9370 = getelementptr inbounds i8, i8* %frontier.95, i64 64
  %t9371 = bitcast i8* %t9370 to i8**
  store i8* %t9336, i8** %t9371, align 8
  %t9374 = getelementptr inbounds i8, i8* %frontier.95, i64 80
  %t9378 = getelementptr inbounds i8, i8* %frontier.95, i64 72
  %t9379 = bitcast i8* %t9378 to i64*
  store i64 123, i64* %t9379, align 4
  %t9381 = getelementptr inbounds i8, i8* %frontier.95, i64 96
  %t9384 = bitcast i8* %t9374 to i8**
  %t9386 = getelementptr inbounds i8, i8* %stackTop.95, i64 40
  %t9387 = bitcast i8* %t9386 to i8**
  %t9388 = load i8*, i8** %t9387, align 8
  store i8* %t9388, i8** %t9384, align 8
  %t9390 = getelementptr inbounds i8, i8* %frontier.95, i64 88
  %t9391 = bitcast i8* %t9390 to i8**
  store i8* %t9354, i8** %t9391, align 8
  %t9394 = getelementptr inbounds i8, i8* %stackTop.95, i64 176
  %t9395 = bitcast i8* %t9394 to i8**
  store i8* %t9374, i8** %t9395, align 8
  %t9398 = getelementptr inbounds i8, i8* %stackTop.95, i64 168
  %t9399 = bitcast i8* %t9398 to i64*
  store i64 62, i64* %t9399, align 4
  store i8* %t9381, i8** %t15197, align 8
  store i8* %t9394, i8** %t15200, align 8
  br label %common.ret

L_1762:                                           ; preds = %L_1283, %L_join_6
  %t9451 = getelementptr inbounds i8, i8* %t9489, i64 -8
  %t9452 = bitcast i8* %t9451 to i64*
  store i64 11, i64* %t9452, align 4
  br label %L_1274

L_join_6:                                         ; preds = %L_1241
  %t95074651 = zext i32 %t9528 to i64
  %t9509 = getelementptr inbounds i8, i8* %stackTop.98, i64 80
  %t9510 = bitcast i8* %t9509 to i64*
  store i64 %t95074651, i64* %t9510, align 4
  %t9479 = getelementptr inbounds i8, i8* %stackTop.98, i64 168
  %t9480 = bitcast i8* %t9479 to i64*
  store i64 102, i64* %t9480, align 4
  %t9482 = getelementptr inbounds i8, i8* %stackTop.98, i64 176
  store i8* %frontier.98, i8** %t15197, align 8
  store i8* %t9482, i8** %t15200, align 8
  %t9489 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 %t95074651, i64 21)
  %t9492 = load i8*, i8** %t15197, align 8
  %t9495 = load i8*, i8** %t15200, align 8
  %t9472 = getelementptr inbounds i8, i8* %t9495, i64 -176
  %t9464 = getelementptr inbounds i8, i8* %t9495, i64 -96
  %t9465 = bitcast i8* %t9464 to i64*
  %t9466 = load i64, i64* %t9465, align 4
  %t9460.not3706 = icmp sgt i64 %t9466, 0
  br i1 %t9460.not3706, label %L_1281.lr.ph, label %L_1762

L_1281.lr.ph:                                     ; preds = %L_join_6
  %t9081 = getelementptr inbounds i8, i8* %t9495, i64 -152
  %t9082 = bitcast i8* %t9081 to i32*
  %t9061 = getelementptr inbounds i8, i8* %t9495, i64 -128
  %t9062 = bitcast i8* %t9061 to i64*
  %t9035 = getelementptr inbounds i8, i8* %t9495, i64 -104
  %t9036 = bitcast i8* %t9035 to i8**
  br label %L_1281

L_1241:                                           ; preds = %L_1240
  %t9522.not = icmp eq i32 %t9528, 0
  br i1 %t9522.not, label %L_1288, label %L_join_6

L_1240:                                           ; preds = %L_1237
  %t9528 = load i32, i32* %t9582, align 4
  %trunc2100 = icmp sgt i32 %t9528, -1
  br i1 %trunc2100, label %L_1241, label %L_1289

L_1239:                                           ; preds = %L_1237
  %t9536 = getelementptr inbounds i8, i8* %stackTop.98, i64 160
  %t9537 = bitcast i8* %t9536 to i8**
  %t9538 = load i8*, i8** %t9537, align 8
  store i8* %t9538, i8** %t9585, align 8
  %t9540 = getelementptr inbounds i8, i8* %stackTop.98, i64 56
  br label %L_981

L_1237:                                           ; preds = %L_1236
  %t9549 = add i32 %t9592, -1
  store i32 %t9549, i32* %t13969, align 4
  %t9556 = load i64, i64* %t9600, align 4
  %t9557 = trunc i64 %t9556 to i32
  %t9560 = bitcast i8* %t9584 to i32*
  store i32 %t9557, i32* %t9560, align 4
  %t9565 = sext i32 %t9557 to i64
  %t9571.not = icmp eq i64 %t9556, %t9565
  br i1 %t9571.not, label %L_1240, label %L_1239

L_1236:                                           ; preds = %L_1310, %L_join_5
  %t9575 = getelementptr inbounds i8, i8* %stackTop.98, i64 72
  %t9576 = bitcast i8* %t9575 to i8**
  %t9577 = load i8*, i8** %t9576, align 8
  %t9578 = getelementptr inbounds i8, i8* %t9577, i64 -8
  %t9579 = bitcast i8* %t9578 to i64*
  store i64 11, i64* %t9579, align 4
  %t9582 = bitcast i8* %t9606 to i32*
  %t9584 = getelementptr inbounds i8, i8* %stackTop.98, i64 24
  %t9585 = bitcast i8* %t9584 to i8**
  %88 = bitcast i8* %t9584 to i32**
  %t95862097 = load i32*, i32** %88, align 8
  %t9589 = load i32, i32* %t95862097, align 4
  store i32 %t9589, i32* %t9582, align 4
  %t9592 = load i32, i32* %t13969, align 4
  %t9594.not = icmp eq i32 %t9592, 0
  br i1 %t9594.not, label %L_1309, label %L_1237

L_join_5:                                         ; preds = %L_1234, %L_nonZeroLen_10
  %t9637.sink = phi i8* [ %t9637, %L_nonZeroLen_10 ], [ %stackTop.0, %L_1234 ]
  %t9631.sink = phi i8* [ %t9631, %L_nonZeroLen_10 ], [ getelementptr (i8, i8* @staticHeapM, i64 64), %L_1234 ]
  %stackTop.98 = phi i8* [ %t9610, %L_nonZeroLen_10 ], [ %t9741, %L_1234 ]
  %frontier.98 = phi i8* [ %t9634, %L_nonZeroLen_10 ], [ %frontier.0, %L_1234 ]
  %t9613 = getelementptr inbounds i8, i8* %t9637.sink, i64 -104
  %t9614 = bitcast i8* %t9613 to i8**
  store i8* %t9631.sink, i8** %t9614, align 8
  %t9606 = getelementptr inbounds i8, i8* %stackTop.98, i64 64
  %t9607 = bitcast i8* %t9606 to i8**
  %t9608 = load i8*, i8** %t9607, align 8
  %t9599 = getelementptr inbounds i8, i8* %stackTop.98, i64 48
  %t9600 = bitcast i8* %t9599 to i64*
  %t96013698 = load i64, i64* %t9600, align 4
  %t9602.not3699 = icmp sgt i64 %t96013698, 0
  br i1 %t9602.not3699, label %L_1310.lr.ph, label %L_1236

L_1310.lr.ph:                                     ; preds = %L_join_5
  %t8582 = getelementptr inbounds i8, i8* %stackTop.98, i64 72
  %t8583 = bitcast i8* %t8582 to i8**
  br label %L_1310

L_nonZeroLen_10:                                  ; preds = %L_1234
  %t9621 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t9622 = bitcast i8* %t9621 to i64*
  store i64 101, i64* %t9622, align 4
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %stackTop.0, i8** %t15200, align 8
  %t9631 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t9662, i64 21)
  %t9634 = load i8*, i8** %t15197, align 8
  %t9637 = load i8*, i8** %t15200, align 8
  %t9610 = getelementptr inbounds i8, i8* %t9637, i64 -176
  br label %L_join_5

L_1234:                                           ; preds = %L_1761
  %t9642.not = icmp eq i32 %TW32_0.21, 0
  br i1 %t9642.not, label %L_join_5, label %L_nonZeroLen_10

L_1761:                                           ; preds = %loop_67
  %t9646 = getelementptr inbounds i8, i8* %stackTop.0, i64 -112
  %t9647 = bitcast i8* %t9646 to i8**
  store i8* %t9660, i8** %t9647, align 8
  %t9653 = icmp sgt i32 %TW32_0.21, -1
  br i1 %t9653, label %L_1234, label %L_1311

loop_67:                                          ; preds = %loop_67, %L_1229
  %TW32_0.21 = phi i32 [ 0, %L_1229 ], [ %t9657, %loop_67 ]
  %t9662 = sext i32 %TW32_0.21 to i64
  store i64 %t9662, i64* %t9665, align 4
  %t9672 = getelementptr inbounds i8, i8* %t9660, i64 %t9662
  %t9675 = load i8, i8* %t9672, align 1
  %cond56 = icmp eq i8 %t9675, 0
  %t9657 = add i32 %TW32_0.21, 1
  br i1 %cond56, label %L_1761, label %loop_67

L_1229.fold.split:                                ; preds = %L_1220
  br label %L_1229

L_1229.fold.split2639:                            ; preds = %L_1220
  br label %L_1229

L_1229.fold.split2640:                            ; preds = %L_1220
  br label %L_1229

L_1229:                                           ; preds = %L_1220, %L_1229.fold.split2640, %L_1229.fold.split2639, %L_1229.fold.split
  %TW32_0.22 = phi i32 [ 1, %L_1220 ], [ 0, %L_1229.fold.split ], [ 2, %L_1229.fold.split2639 ], [ 3, %L_1229.fold.split2640 ]
  %t9680 = load i32, i32* %t13969, align 4
  %t9681 = add i32 %t9680, 1
  store i32 %t9681, i32* %t13969, align 4
  %t9688 = getelementptr inbounds i8, i8* %stackTop.0, i64 -152
  %t9689 = bitcast i8* %t9688 to i8**
  %t9690 = load i8*, i8** %t9689, align 8
  %t9691 = tail call i64 @Real64_gdtoa(double %t9754, i32 3, i32 10, i32 %TW32_0.22, i8* %t9690)
  %t9660 = inttoptr i64 %t9691 to i8*
  %t9664 = getelementptr inbounds i8, i8* %stackTop.0, i64 -128
  %t9665 = bitcast i8* %t9664 to i64*
  br label %loop_67

L_1227:                                           ; preds = %L_1220
  %t9693 = getelementptr inbounds i8, i8* %stackTop.0, i64 -152
  %t9694 = bitcast i8* %t9693 to i8**
  %t9696 = getelementptr inbounds i8, i8* %stackTop.0, i64 -16
  %t9697 = bitcast i8* %t9696 to i8**
  %t9698 = load i8*, i8** %t9697, align 8
  store i8* %t9698, i8** %t9694, align 8
  %t9700 = getelementptr inbounds i8, i8* %stackTop.0, i64 -120
  br label %L_981

L_1220:                                           ; preds = %L_1760
  %t9736 = load double, double* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 9232) to double*), align 8
  %t9737 = fcmp uge double %t9754, %t9736
  %.sink5217 = select i1 %t9737, i8* getelementptr (i8, i8* @staticHeapI, i64 3816), i8* getelementptr (i8, i8* @staticHeapI, i64 3496)
  %t9725 = getelementptr inbounds i8, i8* %stackTop.0, i64 -136
  %t9726 = bitcast i8* %t9725 to i8**
  store i8* %.sink5217, i8** %t9726, align 8
  %t9723 = tail call i32 @IEEEReal_getRoundingMode()
  switch i32 %t9723, label %L_1227 [
    i32 0, label %L_1229
    i32 1024, label %L_1229.fold.split2640
    i32 2048, label %L_1229.fold.split2639
    i32 3072, label %L_1229.fold.split
  ]

L_1760:                                           ; preds = %doSwitchNextBlock
  %t9741 = getelementptr inbounds i8, i8* %stackTop.0, i64 -176
  %t9744 = bitcast i8* %stackTop.0 to double*
  %t9745 = load double, double* %t9744, align 8
  %t9753 = load double, double* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 9240) to double*), align 8
  %t9754 = fmul double %t9745, %t9753
  %t9756 = bitcast double %t9754 to i64
  %t9758 = and i64 %t9756, 9218868437227405312
  %t9760.not = icmp eq i64 %t9758, 9218868437227405312
  br i1 %t9760.not, label %L_1319, label %L_1220

L_1218:                                           ; preds = %L_1214, %L_1333, %L_1216, %L_1340
  %TP_0.44 = phi i8* [ %t8385, %L_1340 ], [ %t9807, %L_1216 ], [ inttoptr (i64 1 to i8*), %L_1333 ], [ %t9854, %L_1214 ]
  %stackTop.99 = phi i8* [ %stackTop.88, %L_1340 ], [ %stackTop.100, %L_1216 ], [ %stackTop.100, %L_1333 ], [ %stackTop.100, %L_1214 ]
  %frontier.99 = phi i8* [ %t8388, %L_1340 ], [ %frontier.100, %L_1216 ], [ %frontier.100, %L_1333 ], [ %frontier.100, %L_1214 ]
  %t9764 = getelementptr inbounds i8, i8* %stackTop.99, i64 176
  %t9765 = bitcast i8* %t9764 to i8**
  store i8* %TP_0.44, i8** %t9765, align 8
  %t9768 = getelementptr inbounds i8, i8* %stackTop.99, i64 184
  %t9769 = bitcast i8* %t9768 to i8**
  %t9771 = getelementptr inbounds i8, i8* %stackTop.99, i64 40
  %t9772 = bitcast i8* %t9771 to i8**
  %t9773 = load i8*, i8** %t9772, align 8
  store i8* %t9773, i8** %t9769, align 8
  %t9776 = getelementptr inbounds i8, i8* %stackTop.99, i64 192
  %t9777 = bitcast i8* %t9776 to i64*
  store i64 8, i64* %t9777, align 4
  %t9779 = getelementptr inbounds i8, i8* %stackTop.99, i64 200
  %t9780 = bitcast i8* %t9779 to i8**
  %t9782 = getelementptr inbounds i8, i8* %stackTop.99, i64 104
  %t9783 = bitcast i8* %t9782 to i8**
  %t9784 = load i8*, i8** %t9783, align 8
  store i8* %t9784, i8** %t9780, align 8
  %t9786 = getelementptr inbounds i8, i8* %stackTop.99, i64 208
  %t9787 = bitcast i8* %t9786 to i8**
  %t9789 = getelementptr inbounds i8, i8* %stackTop.99, i64 64
  %t9790 = bitcast i8* %t9789 to i8**
  %t9791 = load i8*, i8** %t9790, align 8
  store i8* %t9791, i8** %t9787, align 8
  %t9794 = getelementptr inbounds i8, i8* %stackTop.99, i64 168
  br label %L_213.sink.split

L_1216:                                           ; preds = %L_1215
  %t9800 = sdiv i64 %t9822, %t9829
  %t9803 = shl i64 %t9800, 1
  %t9805 = or i64 %t9803, 1
  %t9807 = inttoptr i64 %t9805 to i8*
  br label %L_1218

L_1215:                                           ; preds = %L_1214
  %t9813.not = icmp ult i8* %t9850, inttoptr (i64 2 to i8*)
  br i1 %t9813.not, label %L_1326, label %L_1216

L_1214:                                           ; preds = %x_6
  %t9852 = getelementptr inbounds i8, i8* %t9840, i64 8
  %t9853 = bitcast i8* %t9852 to i8**
  %t9854 = load i8*, i8** %t9853, align 8
  %t9822 = ashr i64 %t9868, 1
  %t9829 = ashr i64 %t9863, 1
  %t9831 = icmp ne i64 %t9822, -4611686018427387904
  %t8466 = icmp ne i64 %t9829, -1
  %or.cond2583 = select i1 %t9831, i1 true, i1 %t8466
  br i1 %or.cond2583, label %L_1215, label %L_1218

x_6:                                              ; preds = %L_1213, %L_1363
  %t9867 = phi i8* [ %t8213, %L_1363 ], [ %t9880, %L_1213 ]
  %stackTop.100 = phi i8* [ %stackTop.87, %L_1363 ], [ %t9958, %L_1213 ]
  %frontier.100 = phi i8* [ %t8216, %L_1363 ], [ %frontier.0, %L_1213 ]
  %t9835 = getelementptr inbounds i8, i8* %stackTop.100, i64 48
  %t9836 = bitcast i8* %t9835 to i8**
  %t9837 = load i8*, i8** %t9836, align 8
  %t9838 = getelementptr inbounds i8, i8* %t9837, i64 8
  %t9839 = bitcast i8* %t9838 to i8**
  %t9840 = load i8*, i8** %t9839, align 8
  %t9842 = getelementptr inbounds i8, i8* %stackTop.100, i64 80
  %t9843 = bitcast i8* %t9842 to i8**
  %t9849 = bitcast i8* %t9837 to i8**
  %t9850 = load i8*, i8** %t9849, align 8
  store i8* %t9850, i8** %t9843, align 8
  %t9857 = bitcast i8* %t9840 to i64*
  %t9858 = load i64, i64* %t9857, align 4
  %t9863 = ptrtoint i8* %t9850 to i64
  %t9868 = ptrtoint i8* %t9867 to i64
  %t9869 = and i64 %t9863, 1
  %89 = and i64 %t9869, %t9868
  %.not2070.not = icmp eq i64 %89, 0
  br i1 %.not2070.not, label %L_1329, label %L_1214

L_1213:                                           ; preds = %L_1211
  %t9885 = or i64 %t9883, 1
  %t9880 = inttoptr i64 %t9885 to i8*
  store i8* %t9880, i8** %t9923.pre-phi, align 8
  br label %x_6

L_1211:                                           ; preds = %L_1210
  %t9899 = ashr i64 %t9925.pre-phi, 1
  %t9906 = ashr i64 %t9920, 1
  %t9909 = sub nsw i64 %t9899, %t9906
  %t9883 = shl i64 %t9909, 1
  %t9888 = xor i64 %t9883, %t9909
  %trunc2084 = icmp sgt i64 %t9888, -1
  br i1 %trunc2084, label %L_1213, label %L_1352

L_1210:                                           ; preds = %L_1209, %L_1373
  %t9925.pre-phi = phi i64 [ %t9964, %L_1209 ], [ %.pre4494.pre-phi, %L_1373 ]
  %t9923.pre-phi = phi i8** [ %t9935, %L_1209 ], [ %t8109, %L_1373 ]
  %t9924 = phi i8* [ %t9962, %L_1209 ], [ %t8113, %L_1373 ]
  %t9919 = phi i8* [ %t9919.pre, %L_1209 ], [ %t8113, %L_1373 ]
  %t9920 = ptrtoint i8* %t9919 to i64
  %t9926 = and i64 %t9920, 1
  %90 = and i64 %t9926, %t9925.pre-phi
  %.not2060.not = icmp eq i64 %90, 0
  br i1 %.not2060.not, label %L_1352, label %L_1211

L_1209:                                           ; preds = %L_1374, %L_1207
  %t9919.pre = phi i8* [ %t9968, %L_1207 ], [ %t9919.pre.pre, %L_1374 ]
  %t9934 = getelementptr inbounds i8, i8* %stackTop.0, i64 -104
  %t9935 = bitcast i8* %t9934 to i8**
  store i8* %t9962, i8** %t9935, align 8
  br label %L_1210

L_1207:                                           ; preds = %L_1759
  %t9944.not = icmp slt i8* %t9962, %t9968
  br i1 %t9944.not, label %L_1373, label %L_1209

L_1759:                                           ; preds = %doSwitchNextBlock
  %t9958 = getelementptr inbounds i8, i8* %stackTop.0, i64 -176
  %t9961 = bitcast i8* %stackTop.0 to i8**
  %t9962 = load i8*, i8** %t9961, align 8
  %t9964 = ptrtoint i8* %t9962 to i64
  %t9966 = getelementptr inbounds i8, i8* %stackTop.0, i64 -80
  %t9967 = bitcast i8* %t9966 to i8**
  %t9968 = load i8*, i8** %t9967, align 8
  %t9969 = ptrtoint i8* %t9968 to i64
  %t9970 = and i64 %t9964, 1
  %91 = and i64 %t9970, %t9969
  %.not2057.not = icmp eq i64 %91, 0
  br i1 %.not2057.not, label %L_1374, label %L_1207

L_1205:                                           ; preds = %L_1758, %L_1773
  %frontier.101.lcssa4606 = phi i8* [ %t8014, %L_1758 ], [ %frontier.101.lcssa4607, %L_1773 ]
  %stackTop.101.lcssa4604 = phi i8* [ %stackTop.85, %L_1758 ], [ %stackTop.101.lcssa4605, %L_1773 ]
  %t9978 = getelementptr inbounds i8, i8* %stackTop.101.lcssa4604, i64 176
  %t9979 = bitcast i8* %t9978 to i64*
  store i64 8, i64* %t9979, align 4
  %t9981 = getelementptr inbounds i8, i8* %stackTop.101.lcssa4604, i64 184
  %t9982 = bitcast i8* %t9981 to i8**
  %t9984 = getelementptr inbounds i8, i8* %stackTop.101.lcssa4604, i64 120
  %t9985 = bitcast i8* %t9984 to i8**
  %t9986 = load i8*, i8** %t9985, align 8
  store i8* %t9986, i8** %t9982, align 8
  %t9988 = getelementptr inbounds i8, i8* %stackTop.101.lcssa4604, i64 192
  %t9989 = bitcast i8* %t9988 to i8**
  %t9991 = getelementptr inbounds i8, i8* %stackTop.101.lcssa4604, i64 112
  %t9992 = bitcast i8* %t9991 to i8**
  %t9993 = load i8*, i8** %t9992, align 8
  store i8* %t9993, i8** %t9989, align 8
  %t9995 = getelementptr inbounds i8, i8* %stackTop.101.lcssa4604, i64 168
  br label %L_638.sink.split

L_1758:                                           ; preds = %L_1203
  %t10001 = getelementptr inbounds i8, i8* %stackTop.85, i64 64
  %t10002 = bitcast i8* %t10001 to i8**
  store i8* %t10021, i8** %t10002, align 8
  br label %L_1205

L_1203:                                           ; preds = %L_1203, %L_1202
  %TP_0.45.in.in = phi i8* [ %t8007, %L_1202 ], [ %TP_0.45, %L_1203 ]
  %TP_0.45.in = bitcast i8* %TP_0.45.in.in to i8**
  %TP_0.45 = load i8*, i8** %TP_0.45.in, align 8
  %cond55 = icmp eq i8* %TP_0.45, inttoptr (i64 1 to i8*)
  br i1 %cond55, label %L_1758, label %L_1203

L_1202:                                           ; preds = %L_1201
  %t10019 = getelementptr inbounds i8, i8* %stackTop.85, i64 88
  %t10020 = bitcast i8* %t10019 to i8**
  %t10021 = load i8*, i8** %t10020, align 8
  br label %L_1203

L_1201:                                           ; preds = %L_1385
  %cond54 = icmp eq i8* %t8007, inttoptr (i64 1 to i8*)
  br i1 %cond54, label %L_1773, label %L_1202

L_1200:                                           ; preds = %L_1441, %L_1197, %L_1440
  %t10074.sink = phi i8* [ %t7460, %L_1440 ], [ %t10074, %L_1197 ], [ %t10074, %L_1441 ]
  %t10046 = getelementptr inbounds i8, i8* %stackTop.0, i64 -80
  %t10047 = bitcast i8* %t10046 to i8**
  store i8* %t10074.sink, i8** %t10047, align 8
  store i8* inttoptr (i64 1 to i8*), i8** %t10079, align 8
  %t10043 = getelementptr inbounds i8, i8* %stackTop.0, i64 -100
  %t10044 = bitcast i8* %t10043 to i32*
  store i32 0, i32* %t10044, align 4
  %t100323576 = getelementptr inbounds i8, i8* %stackTop.0, i64 -104
  %t100333577 = bitcast i8* %t100323576 to i32*
  %t100343578 = load i32, i32* %t100333577, align 4
  %t10035.not3579 = icmp sgt i32 %t100343578, 0
  br i1 %t10035.not3579, label %L_1377, label %L_1773

L_1197:                                           ; preds = %L_1757
  %t10056.not = icmp slt i8* %t10074, %t10080
  br i1 %t10056.not, label %L_1440, label %L_1200

L_1757:                                           ; preds = %doSwitchNextBlock
  %t10070 = getelementptr inbounds i8, i8* %stackTop.0, i64 -176
  %t10073 = bitcast i8* %stackTop.0 to i8**
  %t10074 = load i8*, i8** %t10073, align 8
  %t10076 = ptrtoint i8* %t10074 to i64
  %t10078 = getelementptr inbounds i8, i8* %stackTop.0, i64 -48
  %t10079 = bitcast i8* %t10078 to i8**
  %t10080 = load i8*, i8** %t10079, align 8
  %t10081 = ptrtoint i8* %t10080 to i64
  %t10082 = and i64 %t10076, 1
  %92 = and i64 %t10082, %t10081
  %.not2036.not = icmp eq i64 %92, 0
  br i1 %.not2036.not, label %L_1441, label %L_1197

L_1195:                                           ; preds = %L_1756, %L_1444
  %stackTop.102 = phi i8* [ %stackTop.105, %L_1444 ], [ %stackTop.103, %L_1756 ]
  %frontier.102 = phi i8* [ %frontier.105, %L_1444 ], [ %t10142, %L_1756 ]
  %t10090 = getelementptr inbounds i8, i8* %stackTop.102, i64 176
  %t10091 = bitcast i8* %t10090 to i64*
  store i64 8, i64* %t10091, align 4
  %t10093 = getelementptr inbounds i8, i8* %stackTop.102, i64 184
  %t10094 = bitcast i8* %t10093 to i8**
  %t10096 = getelementptr inbounds i8, i8* %stackTop.102, i64 120
  %t10097 = bitcast i8* %t10096 to i8**
  %t10098 = load i8*, i8** %t10097, align 8
  store i8* %t10098, i8** %t10094, align 8
  %t10100 = getelementptr inbounds i8, i8* %stackTop.102, i64 192
  %t10101 = bitcast i8* %t10100 to i8**
  %t10103 = getelementptr inbounds i8, i8* %stackTop.102, i64 112
  %t10104 = bitcast i8* %t10103 to i8**
  %t10105 = load i8*, i8** %t10104, align 8
  store i8* %t10105, i8** %t10101, align 8
  %t10107 = getelementptr inbounds i8, i8* %stackTop.102, i64 168
  br label %L_638.sink.split

L_1756:                                           ; preds = %L_1192
  %t10112 = getelementptr inbounds i8, i8* %stackTop.103, i64 80
  %t10113 = bitcast i8* %t10112 to i8**
  %t10114 = load i8*, i8** %t10113, align 8
  store i8* %t10135, i8** %t10113, align 8
  %t10120 = getelementptr inbounds i8, i8* %stackTop.103, i64 88
  %t10121 = bitcast i8* %t10120 to i8**
  store i8* %t10114, i8** %t10121, align 8
  br label %L_1195

L_1193:                                           ; preds = %L_1192
  %t10124 = getelementptr inbounds i8, i8* %TP_0.46, i64 8
  %t10125 = bitcast i8* %t10124 to i8**
  %t10126 = load i8*, i8** %t10125, align 8
  %t10129 = bitcast i8* %TP_0.46 to i8**
  %t10130 = load i8*, i8** %t10129, align 8
  br label %L_1189

L_1192:                                           ; preds = %L_1189, %L_1774
  %TP_2.3 = phi i8* [ %t7386, %L_1774 ], [ %TP_2.4, %L_1189 ]
  %TP_1.10 = phi i8* [ %t7390, %L_1774 ], [ %TP_1.11, %L_1189 ]
  %TP_0.46 = phi i8* [ %t7394, %L_1774 ], [ %TP_0.47, %L_1189 ]
  %stackTop.103 = phi i8* [ %t7382, %L_1774 ], [ %stackTop.104, %L_1189 ]
  %frontier.103 = phi i8* [ %t7435, %L_1774 ], [ %frontier.104, %L_1189 ]
  %t10135 = getelementptr inbounds i8, i8* %frontier.103, i64 8
  %t10140 = bitcast i8* %frontier.103 to i64*
  store i64 107, i64* %t10140, align 4
  %t10142 = getelementptr inbounds i8, i8* %frontier.103, i64 24
  %t10145 = bitcast i8* %t10135 to i8**
  store i8* %TP_1.10, i8** %t10145, align 8
  %t10148 = getelementptr inbounds i8, i8* %frontier.103, i64 16
  %t10149 = bitcast i8* %t10148 to i8**
  store i8* %TP_2.3, i8** %t10149, align 8
  %cond39 = icmp eq i8* %TP_0.46, inttoptr (i64 1 to i8*)
  br i1 %cond39, label %L_1756, label %L_1193

L_1189:                                           ; preds = %L_1188, %L_1193
  %TP_2.4 = phi i8* [ inttoptr (i64 1 to i8*), %L_1188 ], [ %t10135, %L_1193 ]
  %TP_1.11 = phi i8* [ %t10173, %L_1188 ], [ %t10130, %L_1193 ]
  %TP_0.47 = phi i8* [ %t10166, %L_1188 ], [ %t10126, %L_1193 ]
  %stackTop.104 = phi i8* [ %stackTop.105, %L_1188 ], [ %stackTop.103, %L_1193 ]
  %frontier.104 = phi i8* [ %frontier.105, %L_1188 ], [ %t10142, %L_1193 ]
  %t10155 = load i8*, i8** %t14203, align 8
  %t10157.not = icmp ult i8* %t10155, %frontier.104
  br i1 %t10157.not, label %L_1774, label %L_1192

L_1188:                                           ; preds = %L_1754
  %t10164 = getelementptr inbounds i8, i8* %t10188, i64 8
  %t10165 = bitcast i8* %t10164 to i8**
  %t10166 = load i8*, i8** %t10165, align 8
  %t10172 = bitcast i8* %t10188 to i8**
  %t10173 = load i8*, i8** %t10172, align 8
  %t10175 = getelementptr inbounds i8, i8* %stackTop.105, i64 80
  %t10176 = bitcast i8* %t10175 to i8**
  store i8* %t10184, i8** %t10176, align 8
  br label %L_1189

L_1754:                                           ; preds = %loop_65
  %t10182 = getelementptr inbounds i8, i8* %stackTop.105, i64 152
  %t10183 = bitcast i8* %t10182 to i8**
  %t10184 = load i8*, i8** %t10183, align 8
  %cond38 = icmp eq i8* %t10188, inttoptr (i64 1 to i8*)
  br i1 %cond38, label %L_1444, label %L_1188

loop_65:                                          ; preds = %L_1186, %L_1457
  %t10188 = phi i8* [ inttoptr (i64 1 to i8*), %L_1186 ], [ %t7253, %L_1457 ]
  %TW32_0.23 = phi i32 [ 0, %L_1186 ], [ %t7279, %L_1457 ]
  %stackTop.105 = phi i8* [ %stackTop.106, %L_1186 ], [ %stackTop.72, %L_1457 ]
  %frontier.105 = phi i8* [ %frontier.106, %L_1186 ], [ %t7260, %L_1457 ]
  %t10192 = getelementptr inbounds i8, i8* %stackTop.105, i64 72
  %t10193 = bitcast i8* %t10192 to i32*
  %t10194 = load i32, i32* %t10193, align 4
  %t10195.not = icmp slt i32 %TW32_0.23, %t10194
  br i1 %t10195.not, label %L_1445, label %L_1754

L_1186:                                           ; preds = %L_1753, %L_1464
  %t10203.pre-phi = phi i8** [ %t10203.phi.trans.insert, %L_1753 ], [ %t10296, %L_1464 ]
  %t10211 = phi i8* [ %t10211.pre, %L_1753 ], [ %t10307, %L_1464 ]
  %t10204 = phi i8* [ %t10204.pre, %L_1753 ], [ %t10300, %L_1464 ]
  %stackTop.106 = phi i8* [ %stackTop.107, %L_1753 ], [ %stackTop.109, %L_1464 ]
  %frontier.106 = phi i8* [ %t10247, %L_1753 ], [ %frontier.109, %L_1464 ]
  %t10199 = getelementptr inbounds i8, i8* %stackTop.106, i64 80
  %t10200 = bitcast i8* %t10199 to i8**
  store i8* %t10204, i8** %t10200, align 8
  %t10206 = getelementptr inbounds i8, i8* %stackTop.106, i64 152
  %t10207 = bitcast i8* %t10206 to i8**
  store i8* %t10211, i8** %t10207, align 8
  store i8* inttoptr (i64 1 to i8*), i8** %t10203.pre-phi, align 8
  br label %loop_65

L_1753:                                           ; preds = %L_1183
  %t10217 = getelementptr inbounds i8, i8* %stackTop.107, i64 64
  %t10218 = bitcast i8* %t10217 to i8**
  store i8* %t10240, i8** %t10218, align 8
  %t10202.phi.trans.insert = getelementptr inbounds i8, i8* %stackTop.107, i64 88
  %t10203.phi.trans.insert = bitcast i8* %t10202.phi.trans.insert to i8**
  %t10204.pre = load i8*, i8** %t10203.phi.trans.insert, align 8
  %t10209.phi.trans.insert = getelementptr inbounds i8, i8* %stackTop.107, i64 168
  %t10210.phi.trans.insert = bitcast i8* %t10209.phi.trans.insert to i8**
  %t10211.pre = load i8*, i8** %t10210.phi.trans.insert, align 8
  br label %L_1186

L_1183:                                           ; preds = %L_1180, %L_1779
  %TP_2.5 = phi i8* [ %t6949, %L_1779 ], [ %TP_2.6, %L_1180 ]
  %TP_1.12 = phi i8* [ %t6953, %L_1779 ], [ %TP_1.13, %L_1180 ]
  %TP_0.48 = phi i8* [ %t6957, %L_1779 ], [ %TP_0.49, %L_1180 ]
  %stackTop.107 = phi i8* [ %t6945, %L_1779 ], [ %stackTop.108, %L_1180 ]
  %frontier.107 = phi i8* [ %t7009, %L_1779 ], [ %frontier.108, %L_1180 ]
  %t10240 = getelementptr inbounds i8, i8* %frontier.107, i64 8
  %t10245 = bitcast i8* %frontier.107 to i64*
  store i64 107, i64* %t10245, align 4
  %t10247 = getelementptr inbounds i8, i8* %frontier.107, i64 24
  %t10250 = bitcast i8* %t10240 to i8**
  store i8* %TP_1.12, i8** %t10250, align 8
  %t10253 = getelementptr inbounds i8, i8* %frontier.107, i64 16
  %t10254 = bitcast i8* %t10253 to i8**
  store i8* %TP_2.5, i8** %t10254, align 8
  %cond35 = icmp eq i8* %TP_0.48, inttoptr (i64 1 to i8*)
  br i1 %cond35, label %L_1753, label %L_1180

L_1180:                                           ; preds = %L_1751, %L_1183
  %TP_2.6 = phi i8* [ %t10240, %L_1183 ], [ inttoptr (i64 1 to i8*), %L_1751 ]
  %TP_1.13.in.in = phi i8* [ %TP_0.48, %L_1183 ], [ %t10311, %L_1751 ]
  %stackTop.108 = phi i8* [ %stackTop.107, %L_1183 ], [ %stackTop.109, %L_1751 ]
  %frontier.108 = phi i8* [ %t10247, %L_1183 ], [ %frontier.109, %L_1751 ]
  %TP_0.49.in.in = getelementptr inbounds i8, i8* %TP_1.13.in.in, i64 8
  %TP_0.49.in = bitcast i8* %TP_0.49.in.in to i8**
  %TP_0.49 = load i8*, i8** %TP_0.49.in, align 8
  %TP_1.13.in = bitcast i8* %TP_1.13.in.in to i8**
  %TP_1.13 = load i8*, i8** %TP_1.13.in, align 8
  %t10260 = load i8*, i8** %t14203, align 8
  %t10262.not = icmp ult i8* %t10260, %frontier.108
  br i1 %t10262.not, label %L_1779, label %L_1183

L_1751:                                           ; preds = %loop_64
  %t10295 = getelementptr inbounds i8, i8* %stackTop.109, i64 88
  %t10296 = bitcast i8* %t10295 to i8**
  %t10298 = getelementptr inbounds i8, i8* %stackTop.109, i64 64
  %t10299 = bitcast i8* %t10298 to i8**
  %t10300 = load i8*, i8** %t10299, align 8
  store i8* %t10300, i8** %t10296, align 8
  %t10302 = getelementptr inbounds i8, i8* %stackTop.109, i64 168
  %t10303 = bitcast i8* %t10302 to i8**
  %t10305 = getelementptr inbounds i8, i8* %stackTop.109, i64 152
  %t10306 = bitcast i8* %t10305 to i8**
  %t10307 = load i8*, i8** %t10306, align 8
  store i8* %t10307, i8** %t10303, align 8
  %cond34 = icmp eq i8* %t10311, inttoptr (i64 1 to i8*)
  br i1 %cond34, label %L_1464, label %L_1180

loop_64:                                          ; preds = %L_1177, %L_1477
  %t10311 = phi i8* [ inttoptr (i64 1 to i8*), %L_1177 ], [ %t6820, %L_1477 ]
  %t10317 = phi i32 [ %t10409, %L_1177 ], [ %t10317.pre, %L_1477 ]
  %TW32_0.24 = phi i32 [ 0, %L_1177 ], [ %t6846, %L_1477 ]
  %stackTop.109 = phi i8* [ %stackTop.110, %L_1177 ], [ %stackTop.66, %L_1477 ]
  %frontier.109 = phi i8* [ %t10399, %L_1177 ], [ %t6827, %L_1477 ]
  %t10318.not = icmp slt i32 %TW32_0.24, %t10317
  br i1 %t10318.not, label %L_1465, label %L_1751

L_1177:                                           ; preds = %L_1176
  %t10323 = bitcast i8* %t10382 to i8**
  %t10325 = getelementptr inbounds i8, i8* %stackTop.110, i64 80
  %t10326 = bitcast i8* %t10325 to i8**
  %t10327 = load i8*, i8** %t10326, align 8
  store i8* %t10327, i8** %t10323, align 8
  %t10329 = getelementptr inbounds i8, i8* %stackTop.110, i64 152
  %t10330 = bitcast i8* %t10329 to i8**
  %t10332 = getelementptr inbounds i8, i8* %stackTop.110, i64 88
  %t10333 = bitcast i8* %t10332 to i8**
  %t10334 = load i8*, i8** %t10333, align 8
  store i8* %t10334, i8** %t10330, align 8
  store i8* inttoptr (i64 1 to i8*), i8** %t10326, align 8
  br label %loop_64

L_1176:                                           ; preds = %L_1750, %L_1174
  %stackTop.110 = phi i8* [ %t6549, %L_1174 ], [ %t10434, %L_1750 ]
  %frontier.110 = phi i8* [ %t6564, %L_1174 ], [ %t10467, %L_1750 ]
  %t10340 = getelementptr inbounds i8, i8* %frontier.110, i64 8
  %t10342 = getelementptr inbounds i8, i8* %stackTop.110, i64 96
  %t10343 = bitcast i8* %t10342 to i8**
  store i8* %t10340, i8** %t10343, align 8
  %t10351 = bitcast i8* %frontier.110 to i64*
  store i64 19, i64* %t10351, align 4
  %93 = bitcast i8* %t10342 to i32**
  %t103571993 = load i32*, i32** %93, align 8
  store i32 0, i32* %t103571993, align 4
  %t10361 = getelementptr inbounds i8, i8* %frontier.110, i64 24
  %t10363 = getelementptr inbounds i8, i8* %stackTop.110, i64 136
  %t10364 = bitcast i8* %t10363 to i8**
  store i8* %t10361, i8** %t10364, align 8
  %t10371 = getelementptr inbounds i8, i8* %frontier.110, i64 16
  %t10372 = bitcast i8* %t10371 to i64*
  store i64 19, i64* %t10372, align 4
  %94 = bitcast i8* %t10363 to i32**
  %t103781994 = load i32*, i32** %94, align 8
  %t10382 = getelementptr inbounds i8, i8* %stackTop.110, i64 64
  %t10383 = bitcast i8* %t10382 to i32*
  %t10384 = load i32, i32* %t10383, align 4
  store i32 %t10384, i32* %t103781994, align 4
  %t10386 = getelementptr inbounds i8, i8* %frontier.110, i64 40
  %t10388 = getelementptr inbounds i8, i8* %stackTop.110, i64 144
  %t10389 = bitcast i8* %t10388 to i8**
  store i8* %t10386, i8** %t10389, align 8
  %t10396 = getelementptr inbounds i8, i8* %frontier.110, i64 32
  %t10397 = bitcast i8* %t10396 to i64*
  store i64 35, i64* %t10397, align 4
  %t10399 = getelementptr inbounds i8, i8* %frontier.110, i64 48
  %95 = bitcast i8* %t10388 to i32**
  %t104031995 = load i32*, i32** %95, align 8
  store i32 0, i32* %t104031995, align 4
  %t10407 = getelementptr inbounds i8, i8* %stackTop.110, i64 72
  %t10408 = bitcast i8* %t10407 to i32*
  %t10409 = load i32, i32* %t10408, align 4
  %trunc1996 = icmp sgt i32 %t10409, -1
  br i1 %trunc1996, label %L_1177, label %L_1484

L_1750:                                           ; preds = %L_1486, %L_1171
  %t10415 = bitcast i8* %t10438 to i8**
  store i8* %t10464, i8** %t10415, align 8
  store i8* %t10444, i8** %t10447, align 8
  %t10423 = load i8*, i8** %t14203, align 8
  %t10425.not = icmp ult i8* %t10423, %t10467
  br i1 %t10425.not, label %L_1174, label %L_1176

L_1171:                                           ; preds = %L_1170.thread, %L_1170
  %t104722610 = phi i64 [ 0, %L_1170.thread ], [ %t10472, %L_1170 ]
  %t10454 = getelementptr inbounds i8, i8* %stackTop.111, i64 168
  %t10455 = bitcast i8* %t10454 to i64*
  store i64 100, i64* %t10455, align 4
  %t10457 = getelementptr inbounds i8, i8* %stackTop.111, i64 176
  store i8* %t10569, i8** %t15197, align 8
  store i8* %t10457, i8** %t15200, align 8
  %t10464 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t104722610, i64 73)
  %t10467 = load i8*, i8** %t15197, align 8
  %t10470 = load i8*, i8** %t15200, align 8
  %t10434 = getelementptr inbounds i8, i8* %t10470, i64 -176
  %t10438 = getelementptr inbounds i8, i8* %t10470, i64 -96
  %t10439 = bitcast i8* %t10438 to i64*
  %t10440 = load i64, i64* %t10439, align 4
  %t10442 = getelementptr inbounds i8, i8* %t10470, i64 -80
  %t10443 = bitcast i8* %t10442 to i8**
  %t10444 = load i8*, i8** %t10443, align 8
  %t10446 = getelementptr inbounds i8, i8* %t10470, i64 -88
  %t10447 = bitcast i8* %t10446 to i8**
  %t10430.not3594 = icmp sgt i64 %t10440, 0
  br i1 %t10430.not3594, label %L_1485.preheader, label %L_1750

L_1485.preheader:                                 ; preds = %L_1171
  %t10448 = load i8*, i8** %t10447, align 8
  br label %L_1485

L_1170:                                           ; preds = %L_1167
  %t10472 = sext i32 %t10492 to i64
  %t10475 = bitcast i8* %t10510 to i64*
  store i64 %t10472, i64* %t10475, align 4
  %trunc1990 = icmp sgt i32 %t10492, -1
  br i1 %trunc1990, label %L_1171, label %L_1488

L_1167:                                           ; preds = %L_1748, %L_1167
  %t10516.pn = phi i8* [ %TP_0.51, %L_1167 ], [ %t10562, %L_1748 ]
  %TW32_0.26 = phi i32 [ %t10492, %L_1167 ], [ 0, %L_1748 ]
  %TP_0.51.in.in = getelementptr inbounds i8, i8* %t10516.pn, i64 8
  %TP_0.51.in = bitcast i8* %TP_0.51.in.in to i8**
  %TP_0.51 = load i8*, i8** %TP_0.51.in, align 8
  %t10492 = add i32 %TW32_0.26, 1
  %cond30 = icmp eq i8* %TP_0.51, inttoptr (i64 1 to i8*)
  br i1 %cond30, label %L_1170, label %L_1167

L_1748:                                           ; preds = %L_1163
  %t10507 = getelementptr inbounds i8, i8* %stackTop.111, i64 96
  %t10508 = bitcast i8* %t10507 to i8**
  %t10510 = getelementptr inbounds i8, i8* %stackTop.111, i64 80
  %t10511 = bitcast i8* %t10510 to i8**
  %t10512 = load i8*, i8** %t10511, align 8
  store i8* %t10512, i8** %t10508, align 8
  %cond29 = icmp eq i8* %t10562, inttoptr (i64 1 to i8*)
  br i1 %cond29, label %L_1170.thread, label %L_1167

L_1170.thread:                                    ; preds = %L_1748
  %t104752607 = bitcast i8* %t10510 to i64*
  store i64 0, i64* %t104752607, align 4
  br label %L_1171

L_1747:                                           ; preds = %L_1159
  %t10525 = getelementptr inbounds i8, i8* %stackTop.1123590, i64 136
  %t10526 = bitcast i8* %t10525 to i8**
  %t10528 = getelementptr inbounds i8, i8* %stackTop.1123590, i64 80
  %t10529 = bitcast i8* %t10528 to i8**
  %t10530 = load i8*, i8** %t10529, align 8
  store i8* %t10530, i8** %t10526, align 8
  %t10532 = getelementptr inbounds i8, i8* %stackTop.1123590, i64 168
  %t10533 = bitcast i8* %t10532 to i64*
  store i64 99, i64* %t10533, align 4
  %t10535 = getelementptr inbounds i8, i8* %stackTop.1123590, i64 176
  store i8* %frontier.1123591, i8** %t15197, align 8
  store i8* %t10535, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t10545 = load i8*, i8** %t15197, align 8
  %t10548 = load i8*, i8** %t15200, align 8
  %t10519 = getelementptr inbounds i8, i8* %t10548, i64 -176
  br label %L_1162

L_1164:                                           ; preds = %L_1162
  %t10550 = getelementptr inbounds i8, i8* %stackTop.111, i64 24
  %t10551 = bitcast i8* %t10550 to i8**
  %t10553 = getelementptr inbounds i8, i8* %stackTop.111, i64 160
  %t10554 = bitcast i8* %t10553 to i8**
  %t10555 = load i8*, i8** %t10554, align 8
  store i8* %t10555, i8** %t10551, align 8
  %t10557 = getelementptr inbounds i8, i8* %stackTop.111, i64 56
  br label %L_981

L_1163:                                           ; preds = %L_1162
  %t10607 = add nsw i32 %t10606, -1
  %t10562 = getelementptr inbounds i8, i8* %frontier.111, i64 8
  %t10567 = bitcast i8* %frontier.111 to i64*
  store i64 117, i64* %t10567, align 4
  %t10569 = getelementptr inbounds i8, i8* %frontier.111, i64 24
  %t10572 = bitcast i8* %t10562 to i32*
  %t10574 = getelementptr inbounds i8, i8* %stackTop.111, i64 96
  %t10575 = bitcast i8* %t10574 to i32*
  %t10576 = load i32, i32* %t10575, align 4
  store i32 %t10576, i32* %t10572, align 4
  %t10578 = getelementptr inbounds i8, i8* %frontier.111, i64 16
  %t10579 = bitcast i8* %t10578 to i8**
  %t10581 = getelementptr inbounds i8, i8* %stackTop.111, i64 88
  %t10582 = bitcast i8* %t10581 to i8**
  %t10583 = load i8*, i8** %t10582, align 8
  store i8* %t10583, i8** %t10579, align 8
  store i8* %t10562, i8** %t10582, align 8
  store i32 %t10607, i32* %t10605, align 4
  %t10593 = getelementptr inbounds i8, i8* %stackTop.111, i64 64
  %t10594 = bitcast i8* %t10593 to i32*
  %t10595 = load i32, i32* %t10594, align 4
  %t10600 = getelementptr inbounds i8, i8* %stackTop.111, i64 76
  %t10601 = bitcast i8* %t10600 to i32*
  %t10602 = load i32, i32* %t10601, align 4
  store i32 %t10602, i32* %t10594, align 4
  %cond27 = icmp eq i32 %t10607, 0
  br i1 %cond27, label %L_1748, label %L_1152

L_1162:                                           ; preds = %L_1159, %L_1747
  %stackTop.111 = phi i8* [ %t10519, %L_1747 ], [ %stackTop.1123590, %L_1159 ]
  %frontier.111 = phi i8* [ %t10545, %L_1747 ], [ %frontier.1123591, %L_1159 ]
  %t10604 = getelementptr inbounds i8, i8* %stackTop.111, i64 68
  %t10605 = bitcast i8* %t10604 to i32*
  %t10606 = load i32, i32* %t10605, align 4
  %t10613.not = icmp eq i32 %t10606, -2147483648
  br i1 %t10613.not, label %L_1164, label %L_1163

L_1159:                                           ; preds = %loop_62
  %t10618 = load i8*, i8** %t14203, align 8
  %t10620.not = icmp ult i8* %t10618, %frontier.1123591
  br i1 %t10620.not, label %L_1747, label %L_1162

L_1158:                                           ; preds = %loop_62
  %t10624 = getelementptr inbounds i8, i8* %stackTop.1123590, i64 24
  %t10625 = bitcast i8* %t10624 to i8**
  %t10627 = getelementptr inbounds i8, i8* %stackTop.1123590, i64 160
  %t10628 = bitcast i8* %t10627 to i8**
  %t10629 = load i8*, i8** %t10628, align 8
  store i8* %t10629, i8** %t10625, align 8
  %t10631 = getelementptr inbounds i8, i8* %stackTop.1123590, i64 56
  br label %L_981

L_1154:                                           ; preds = %loop_62
  %t10711 = add nsw i32 %TW32_0.28, -1
  %t10694.frozen = freeze i32 %t10694
  %t10702 = udiv i32 %t10694.frozen, 44488
  %96 = mul i32 %t10702, 44488
  %t10695.decomposed = sub i32 %t10694.frozen, %96
  %t10697 = mul nuw nsw i32 %t10695.decomposed, 48271
  %t10704 = mul nuw nsw i32 %t10702, 3399
  %t10707.not = icmp ugt i32 %t10697, %t10704
  %t10686 = xor i32 %t10704, 2147483647
  %97 = sub nsw i32 0, %t10704
  %TW32_0.27.p = select i1 %t10707.not, i32 %97, i32 %t10686
  %TW32_0.27 = add i32 %TW32_0.27.p, %t10697
  %t10643 = shl i32 %t10641, 18
  %t10649 = xor i32 %t10643, %t10641
  %t10652 = lshr i32 %t10649, 13
  %t10655 = xor i32 %t10652, %t10649
  %t10661 = lshr i32 %t10659, 1
  %t10663 = and i32 %t10661, 1073741823
  %t10666 = xor i32 %TW32_0.27, %t10649
  %t10668 = and i32 %t10666, 1073741824
  %t10671 = or i32 %t10668, %t10663
  store i32 %t10655, i32* %t10723, align 4
  store i32 %TW32_0.27, i32* %t10720, align 4
  store i32 %t10671, i32* %t10727, align 4
  br label %loop_62

loop_62:                                          ; preds = %L_1152, %L_1154
  %t10659 = phi i32 [ 0, %L_1152 ], [ %t10671, %L_1154 ]
  %t10641 = phi i32 [ %TW32_0.293589, %L_1152 ], [ %t10655, %L_1154 ]
  %t10694 = phi i32 [ %t10724, %L_1152 ], [ %TW32_0.27, %L_1154 ]
  %TW32_0.28 = phi i32 [ 31, %L_1152 ], [ %t10711, %L_1154 ]
  switch i32 %TW32_0.28, label %L_1154 [
    i32 0, label %L_1159
    i32 -2147483648, label %L_1158
  ]

L_1152:                                           ; preds = %L_1152.preheader, %L_1163
  %t10724 = phi i32 [ %t10602, %L_1163 ], [ %t10724.pre, %L_1152.preheader ]
  %frontier.1123591 = phi i8* [ %t10569, %L_1163 ], [ %frontier.0, %L_1152.preheader ]
  %stackTop.1123590 = phi i8* [ %stackTop.111, %L_1163 ], [ %stackTop.113, %L_1152.preheader ]
  %TW32_0.293589 = phi i32 [ %t10595, %L_1163 ], [ 200, %L_1152.preheader ]
  %t10719 = getelementptr inbounds i8, i8* %stackTop.1123590, i64 76
  %t10720 = bitcast i8* %t10719 to i32*
  %t10722 = getelementptr inbounds i8, i8* %stackTop.1123590, i64 64
  %t10723 = bitcast i8* %t10722 to i32*
  store i32 %t10724, i32* %t10720, align 4
  %t10726 = getelementptr inbounds i8, i8* %stackTop.1123590, i64 96
  %t10727 = bitcast i8* %t10726 to i32*
  store i32 0, i32* %t10727, align 4
  store i32 %TW32_0.293589, i32* %t10723, align 4
  br label %loop_62

L_1152.preheader:                                 ; preds = %L_1494, %L_1150
  %t6448.sink = phi i32 [ %t6448, %L_1494 ], [ %t10764, %L_1150 ]
  %stackTop.113 = phi i8* [ %stackTop.64, %L_1494 ], [ %t10771, %L_1150 ]
  %t6450 = getelementptr inbounds i8, i8* %stackTop.0, i64 -104
  %t6451 = bitcast i8* %t6450 to i32*
  store i32 %t6448.sink, i32* %t6451, align 4
  %t10737 = getelementptr inbounds i8, i8* %stackTop.0, i64 -96
  %t10738 = bitcast i8* %t10737 to i8**
  %t10740 = getelementptr inbounds i8, i8* %stackTop.0, i64 -112
  %t10741 = bitcast i8* %t10740 to i8**
  %t10742 = load i8*, i8** %t10741, align 8
  store i8* %t10742, i8** %t10738, align 8
  %t10745 = bitcast i8* %t10740 to i32*
  store i32 201, i32* %t10745, align 4
  %t10747 = getelementptr inbounds i8, i8* %stackTop.0, i64 -88
  %t10748 = bitcast i8* %t10747 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t10748, align 8
  %t10751 = getelementptr inbounds i8, i8* %stackTop.0, i64 -108
  %t10752 = bitcast i8* %t10751 to i32*
  store i32 48, i32* %t10752, align 4
  %t10722.phi.trans.insert = getelementptr inbounds i8, i8* %stackTop.0, i64 -112
  %t10723.phi.trans.insert = bitcast i8* %t10722.phi.trans.insert to i32*
  %t10724.pre = load i32, i32* %t10723.phi.trans.insert, align 4
  br label %L_1152

L_1150:                                           ; preds = %L_1149
  %t10764 = sub i32 0, %t10762
  br label %L_1152.preheader

L_1149:                                           ; preds = %L_1746
  %t10760 = lshr i64 %t10775, 1
  %t10762 = trunc i64 %t10760 to i32
  %t10766 = tail call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 0, i32 %t10762)
  %t10767 = extractvalue { i32, i1 } %t10766, 1
  br i1 %t10767, label %L_1490, label %L_1150

L_1746:                                           ; preds = %doSwitchNextBlock
  %t10771 = getelementptr inbounds i8, i8* %stackTop.0, i64 -176
  %t10774 = bitcast i8* %stackTop.0 to i64*
  %t10775 = load i64, i64* %t10774, align 4
  %98 = and i64 %t10775, 1
  %trunc1984.not = icmp eq i64 %98, 0
  br i1 %trunc1984.not, label %L_1143, label %L_1149

negate_0:                                         ; preds = %L_1146, %L_1147
  %TW32_0.30 = phi i32 [ %t10841, %L_1147 ], [ %t10824, %L_1146 ]
  br label %L_120.sink.split

L_1147:                                           ; preds = %L_1146
  br label %negate_0

L_1146:                                           ; preds = %L_1144
  %t10813 = load i8*, i8** %t10886, align 8
  %t10816 = getelementptr inbounds i8, i8* %t10813, i64 %t10842
  %t10819 = load i8, i8* %t10816, align 1
  %t10824 = add i32 %t10841, 1
  switch i8 %t10819, label %L_1147 [
    i8 43, label %negate_0
    i8 45, label %L_1492
    i8 126, label %L_120.sink.split
  ]

L_1145:                                           ; preds = %L_1144
  %t10827 = getelementptr inbounds i8, i8* %stackTop.115, i64 24
  %t10828 = bitcast i8* %t10827 to i8**
  %t10830 = getelementptr inbounds i8, i8* %stackTop.115, i64 160
  %t10831 = bitcast i8* %t10830 to i8**
  %t10832 = load i8*, i8** %t10831, align 8
  store i8* %t10832, i8** %t10828, align 8
  %t10834 = getelementptr inbounds i8, i8* %stackTop.115, i64 56
  br label %L_981

L_1144:                                           ; preds = %L_1142
  %t10839 = getelementptr inbounds i8, i8* %stackTop.115, i64 96
  %t10840 = bitcast i8* %t10839 to i32*
  %t10841 = load i32, i32* %t10840, align 4
  %t10842 = sext i32 %t10841 to i64
  %t10845 = getelementptr inbounds i8, i8* %stackTop.115, i64 88
  %t10846 = bitcast i8* %t10845 to i64*
  %t10847 = load i64, i64* %t10846, align 4
  %t10848.not = icmp ugt i64 %t10847, %t10842
  br i1 %t10848.not, label %L_1146, label %L_1145

L_1143:                                           ; preds = %L_1142, %L_1746, %L_1493
  %stackTop.114 = phi i8* [ %stackTop.115, %L_1142 ], [ %t10771, %L_1746 ], [ %stackTop.64, %L_1493 ]
  %frontier.113 = phi i8* [ %t10873, %L_1142 ], [ %frontier.0, %L_1746 ], [ %frontier.0, %L_1493 ]
  %t10852 = getelementptr inbounds i8, i8* %stackTop.114, i64 24
  %t10853 = bitcast i8* %t10852 to i8**
  %t10855 = getelementptr inbounds i8, i8* %stackTop.114, i64 160
  %t10856 = bitcast i8* %t10855 to i8**
  %t10857 = load i8*, i8** %t10856, align 8
  store i8* %t10857, i8** %t10853, align 8
  %t10859 = getelementptr inbounds i8, i8* %stackTop.114, i64 56
  br label %L_981

L_1142:                                           ; preds = %L_1139, %L_1140
  %stackTop.115 = phi i8* [ %t6391, %L_1140 ], [ %stackTop.1172622.sink5218, %L_1139 ]
  %frontier.114 = phi i8* [ %t6406, %L_1140 ], [ %frontier.115, %L_1139 ]
  %t10866 = getelementptr inbounds i8, i8* %frontier.114, i64 8
  %t10871 = bitcast i8* %frontier.114 to i64*
  store i64 77, i64* %t10871, align 4
  %t10873 = getelementptr inbounds i8, i8* %frontier.114, i64 24
  %t10876 = bitcast i8* %t10866 to i8**
  %t10878 = getelementptr inbounds i8, i8* %stackTop.115, i64 72
  %t10879 = bitcast i8* %t10878 to i8**
  %t10880 = load i8*, i8** %t10879, align 8
  store i8* %t10880, i8** %t10876, align 8
  %t10882 = getelementptr inbounds i8, i8* %frontier.114, i64 16
  %t10883 = bitcast i8* %t10882 to i8**
  %t10885 = getelementptr inbounds i8, i8* %stackTop.115, i64 80
  %t10886 = bitcast i8* %t10885 to i8**
  %t10887 = load i8*, i8** %t10886, align 8
  store i8* %t10887, i8** %t10883, align 8
  %t10889 = getelementptr inbounds i8, i8* %stackTop.115, i64 100
  %t10890 = bitcast i8* %t10889 to i32*
  %t10891 = load i32, i32* %t10890, align 4
  %switch2449 = icmp eq i32 %t10891, 0
  br i1 %switch2449, label %L_1143, label %L_1144

L_1139:                                           ; preds = %L_1497, %L_1523, %L_1521, %L_1138
  %t10926.sink = phi i8* [ %t10926, %L_1138 ], [ %t6014, %L_1521 ], [ %t6014, %L_1523 ], [ %t10926, %L_1497 ]
  %t10944.sink = phi i8* [ %t10944, %L_1138 ], [ %t6032, %L_1521 ], [ %t6032, %L_1523 ], [ %t10944, %L_1497 ]
  %stackTop.1172622.sink5218 = phi i8* [ %stackTop.1172622, %L_1138 ], [ %stackTop.612601, %L_1521 ], [ %stackTop.612601, %L_1523 ], [ %stackTop.1172622, %L_1497 ]
  %t10936.sink = phi i8* [ %t10936, %L_1138 ], [ %t6024, %L_1521 ], [ %t6024, %L_1523 ], [ %t10936, %L_1497 ]
  %t10934.sink = phi i8* [ %t10934, %L_1138 ], [ %t6022, %L_1521 ], [ %t6022, %L_1523 ], [ %t10934, %L_1497 ]
  %t10932.sink = phi i32 [ %t10932, %L_1138 ], [ %t6020, %L_1521 ], [ %t6020, %L_1523 ], [ %t10932, %L_1497 ]
  %t10928.sink = phi i32 [ %t10928, %L_1138 ], [ %t6016, %L_1521 ], [ %t6016, %L_1523 ], [ %t10928, %L_1497 ]
  %frontier.115 = phi i8* [ %frontier.1162624, %L_1138 ], [ %frontier.632603, %L_1521 ], [ %frontier.632603, %L_1523 ], [ %frontier.1162624, %L_1497 ]
  %t10906 = bitcast i8* %t10926.sink to i8**
  store i8* %t10944.sink, i8** %t10906, align 8
  %t10909 = getelementptr inbounds i8, i8* %stackTop.1172622.sink5218, i64 80
  %t10910 = bitcast i8* %t10909 to i8**
  store i8* %t10936.sink, i8** %t10910, align 8
  %t10914 = bitcast i8* %t10934.sink to i32*
  store i32 %t10932.sink, i32* %t10914, align 4
  %t10917 = getelementptr inbounds i8, i8* %stackTop.1172622.sink5218, i64 100
  %t10918 = bitcast i8* %t10917 to i32*
  store i32 %t10928.sink, i32* %t10918, align 4
  %t10894 = load i8*, i8** %t14203, align 8
  %t10896.not = icmp ult i8* %t10894, %frontier.115
  br i1 %t10896.not, label %L_1140, label %L_1142

L_1138:                                           ; preds = %L_1741, %L_1132, %L_1137
  %frontier.1162624 = phi i8* [ %t11028, %L_1137 ], [ %t11028, %L_1132 ], [ %frontier.119, %L_1741 ]
  %stackTop.1172622 = phi i8* [ %stackTop.118, %L_1137 ], [ %stackTop.118, %L_1132 ], [ %stackTop.120, %L_1741 ]
  %TP_0.532620 = phi i8* [ %t11021, %L_1137 ], [ inttoptr (i64 1 to i8*), %L_1132 ], [ inttoptr (i64 1 to i8*), %L_1741 ]
  %t109462615 = phi i64 [ %t10946, %L_1137 ], [ 0, %L_1132 ], [ 0, %L_1741 ]
  %t10926 = getelementptr inbounds i8, i8* %stackTop.1172622, i64 72
  %t10927 = bitcast i8* %t10926 to i32*
  %t10928 = load i32, i32* %t10927, align 4
  %t10930 = getelementptr inbounds i8, i8* %stackTop.1172622, i64 76
  %t10931 = bitcast i8* %t10930 to i32*
  %t10932 = load i32, i32* %t10931, align 4
  %t10934 = getelementptr inbounds i8, i8* %stackTop.1172622, i64 96
  %t10935 = bitcast i8* %t10934 to i8**
  %t10936 = load i8*, i8** %t10935, align 8
  %t10942 = getelementptr inbounds i8, i8* %stackTop.1172622, i64 144
  %t10943 = bitcast i8* %t10942 to i8**
  %t10944 = load i8*, i8** %t10943, align 8
  %t10922.not3517 = icmp sgt i64 %t109462615, 0
  br i1 %t10922.not3517, label %L_1496, label %L_1139

L_1137:                                           ; preds = %L_1134.L_1134_crit_edge, %L_1134.preheader
  %t10964.lcssa = phi i32 [ 1, %L_1134.preheader ], [ %t10964, %L_1134.L_1134_crit_edge ]
  %t10946 = sext i32 %t10964.lcssa to i64
  %trunc1982 = icmp sgt i32 %t10964.lcssa, -1
  br i1 %trunc1982, label %L_1138, label %L_1499

L_1134.L_1134_crit_edge:                          ; preds = %L_1134.preheader, %L_1134.L_1134_crit_edge
  %t109645731 = phi i32 [ %t10964, %L_1134.L_1134_crit_edge ], [ 1, %L_1134.preheader ]
  %TP_1.145730 = phi i8* [ %TP_1.14.pre, %L_1134.L_1134_crit_edge ], [ %TP_1.15, %L_1134.preheader ]
  %TP_1.14.in.phi.trans.insert = bitcast i8* %TP_1.145730 to i8**
  %TP_1.14.pre = load i8*, i8** %TP_1.14.in.phi.trans.insert, align 8
  %t10964 = add i32 %t109645731, 1
  %cond25 = icmp eq i8* %TP_1.14.pre, inttoptr (i64 1 to i8*)
  br i1 %cond25, label %L_1137, label %L_1134.L_1134_crit_edge

L_1132:                                           ; preds = %L_1129
  %cond24 = icmp eq i8* %t11021, inttoptr (i64 1 to i8*)
  br i1 %cond24, label %L_1138, label %L_1134.preheader

L_1134.preheader:                                 ; preds = %L_1132
  %cond255729 = icmp eq i8* %TP_1.15, inttoptr (i64 1 to i8*)
  br i1 %cond255729, label %L_1137, label %L_1134.L_1134_crit_edge

L_1130:                                           ; preds = %L_1129
  %t11016 = bitcast i8* %TP_0.54 to i8**
  %t11017 = load i8*, i8** %t11016, align 8
  br label %L_1126

L_1129:                                           ; preds = %L_1126, %L_1786
  %TP_1.15 = phi i8* [ %t6265, %L_1786 ], [ %TP_1.16, %L_1126 ]
  %TP_0.54 = phi i8* [ %t6269, %L_1786 ], [ %TP_0.55, %L_1126 ]
  %stackTop.118 = phi i8* [ %t6261, %L_1786 ], [ %stackTop.119, %L_1126 ]
  %frontier.117 = phi i8* [ %t6350, %L_1786 ], [ %frontier.118, %L_1126 ]
  %t11021 = getelementptr inbounds i8, i8* %frontier.117, i64 8
  %t11026 = bitcast i8* %frontier.117 to i64*
  store i64 99, i64* %t11026, align 4
  %t11028 = getelementptr inbounds i8, i8* %frontier.117, i64 16
  %t11031 = bitcast i8* %t11021 to i8**
  store i8* %TP_1.15, i8** %t11031, align 8
  %cond23 = icmp eq i8* %TP_0.54, inttoptr (i64 1 to i8*)
  br i1 %cond23, label %L_1132, label %L_1130

L_1126:                                           ; preds = %L_1125, %L_1130
  %TP_1.16 = phi i8* [ inttoptr (i64 1 to i8*), %L_1125 ], [ %t11021, %L_1130 ]
  %TP_0.55 = phi i8* [ %t11045, %L_1125 ], [ %t11017, %L_1130 ]
  %stackTop.119 = phi i8* [ %stackTop.120, %L_1125 ], [ %stackTop.118, %L_1130 ]
  %frontier.118 = phi i8* [ %frontier.119, %L_1125 ], [ %t11028, %L_1130 ]
  %t11037 = load i8*, i8** %t14203, align 8
  %t11039.not = icmp ult i8* %t11037, %frontier.118
  br i1 %t11039.not, label %L_1786, label %L_1129

L_1125:                                           ; preds = %L_1741
  %t11044 = bitcast i8* %TP_1.17 to i8**
  %t11045 = load i8*, i8** %t11044, align 8
  %t11048 = bitcast i8* %t11106 to i32*
  store i32 0, i32* %t11048, align 4
  %t11054 = getelementptr inbounds i8, i8* %stackTop.120, i64 84
  %t11055 = bitcast i8* %t11054 to i32*
  store i32 %TW32_0.32, i32* %t11055, align 4
  %t11061 = getelementptr inbounds i8, i8* %stackTop.120, i64 152
  %t11062 = bitcast i8* %t11061 to i8**
  store i8* %TP_0.56, i8** %t11062, align 8
  %t11068 = getelementptr inbounds i8, i8* %stackTop.120, i64 168
  %t11069 = bitcast i8* %t11068 to i8**
  store i8* %t11101, i8** %t11069, align 8
  %t11075 = getelementptr inbounds i8, i8* %stackTop.120, i64 176
  %t11076 = bitcast i8* %t11075 to i8**
  store i8* %t11108, i8** %t11076, align 8
  br label %L_1126

L_1741:                                           ; preds = %L_1123
  %t11083 = getelementptr inbounds i8, i8* %stackTop.120, i64 72
  %t11084 = bitcast i8* %t11083 to i32*
  store i32 0, i32* %t11084, align 4
  %t11087 = getelementptr inbounds i8, i8* %stackTop.120, i64 76
  %t11088 = bitcast i8* %t11087 to i32*
  store i32 %TW32_0.32, i32* %t11088, align 4
  %t11092 = getelementptr inbounds i8, i8* %stackTop.120, i64 96
  %t11093 = bitcast i8* %t11092 to i8**
  store i8* %TP_0.56, i8** %t11093, align 8
  %t11096 = getelementptr inbounds i8, i8* %stackTop.120, i64 136
  %t11097 = bitcast i8* %t11096 to i8**
  %t11099 = getelementptr inbounds i8, i8* %stackTop.120, i64 64
  %t11100 = bitcast i8* %t11099 to i8**
  %t11101 = load i8*, i8** %t11100, align 8
  store i8* %t11101, i8** %t11097, align 8
  %t11103 = getelementptr inbounds i8, i8* %stackTop.120, i64 144
  %t11104 = bitcast i8* %t11103 to i8**
  %t11106 = getelementptr inbounds i8, i8* %stackTop.120, i64 80
  %t11107 = bitcast i8* %t11106 to i8**
  %t11108 = load i8*, i8** %t11107, align 8
  store i8* %t11108, i8** %t11104, align 8
  %cond22 = icmp eq i8* %TP_1.17, inttoptr (i64 1 to i8*)
  br i1 %cond22, label %L_1138, label %L_1125

L_1123:                                           ; preds = %L_1121
  %t11113.not = icmp slt i32 %TW32_0.32, %t11141
  br i1 %t11113.not, label %L_1502, label %L_1741

L_1122:                                           ; preds = %L_1121
  %t11117 = getelementptr inbounds i8, i8* %stackTop.120, i64 24
  %t11118 = bitcast i8* %t11117 to i8**
  %t11120 = getelementptr inbounds i8, i8* %stackTop.120, i64 160
  %t11121 = bitcast i8* %t11120 to i8**
  %t11122 = load i8*, i8** %t11121, align 8
  store i8* %t11122, i8** %t11118, align 8
  %t11124 = getelementptr inbounds i8, i8* %stackTop.120, i64 56
  br label %L_981

L_1121:                                           ; preds = %loop_59, %L_1793
  %TP_1.17 = phi i8* [ %t5780, %L_1793 ], [ %TP_0.57, %loop_59 ]
  %TP_0.56 = phi i8* [ %t5784, %L_1793 ], [ %TP_1.18, %loop_59 ]
  %TW32_0.32 = phi i32 [ %t5776, %L_1793 ], [ %TW32_0.33, %loop_59 ]
  %stackTop.120 = phi i8* [ %t5772, %L_1793 ], [ %stackTop.121, %loop_59 ]
  %frontier.119 = phi i8* [ %t5836, %L_1793 ], [ %frontier.120, %loop_59 ]
  %t11131 = getelementptr inbounds i8, i8* %stackTop.120, i64 88
  %t11132 = bitcast i8* %t11131 to i64*
  %t11134 = getelementptr inbounds i8, i8* %TP_0.56, i64 -16
  %t11135 = bitcast i8* %t11134 to i64*
  %t11136 = load i64, i64* %t11135, align 4
  store i64 %t11136, i64* %t11132, align 4
  %t11141 = trunc i64 %t11136 to i32
  %t11143 = sext i32 %t11141 to i64
  %t11149.not = icmp eq i64 %t11136, %t11143
  br i1 %t11149.not, label %L_1123, label %L_1122

loop_59:                                          ; preds = %L_1118, %L_1528
  %TP_1.18 = phi i8* [ %t11165, %L_1118 ], [ %TP_0.56, %L_1528 ]
  %TP_0.57 = phi i8* [ inttoptr (i64 1 to i8*), %L_1118 ], [ %t5841, %L_1528 ]
  %TW32_0.33 = phi i32 [ 0, %L_1118 ], [ %t6230, %L_1528 ]
  %stackTop.121 = phi i8* [ %stackTop.122, %L_1118 ], [ %stackTop.120, %L_1528 ]
  %frontier.120 = phi i8* [ %frontier.121, %L_1118 ], [ %t5848, %L_1528 ]
  %t11157 = load i8*, i8** %t14203, align 8
  %t11159.not = icmp ult i8* %t11157, %frontier.120
  br i1 %t11159.not, label %L_1793, label %L_1121

L_1118:                                           ; preds = %L_1116
  %t11164 = bitcast i8* %TP_1.19 to i8**
  %t11165 = load i8*, i8** %t11164, align 8
  br label %loop_59

L_1117:                                           ; preds = %L_1116
  %t11178 = getelementptr inbounds i8, i8* %stackTop.122, i64 24
  %t11179 = bitcast i8* %t11178 to i8**
  %t11181 = getelementptr inbounds i8, i8* %stackTop.122, i64 160
  %t11182 = bitcast i8* %t11181 to i8**
  %t11183 = load i8*, i8** %t11182, align 8
  store i8* %t11183, i8** %t11179, align 8
  %t11185 = getelementptr inbounds i8, i8* %stackTop.122, i64 56
  br label %L_981

L_1116:                                           ; preds = %L_1115
  %t11191 = and i64 %t11229, 3
  %cond16 = icmp eq i64 %t11191, 0
  br i1 %cond16, label %L_1118, label %L_1117

L_1115:                                           ; preds = %L_1112
  %t11194 = icmp ult i32 %TW32_0.35, 2147483647
  br i1 %t11194, label %L_1116, label %L_1529

L_1112:                                           ; preds = %L_1738, %L_1112
  %TP_1.19.pn = phi i8* [ %TP_0.59, %L_1112 ], [ %TP_1.19, %L_1738 ]
  %TW32_0.35 = phi i32 [ %t11208, %L_1112 ], [ 0, %L_1738 ]
  %TP_0.59.in.in = getelementptr inbounds i8, i8* %TP_1.19.pn, i64 8
  %TP_0.59.in = bitcast i8* %TP_0.59.in.in to i8**
  %TP_0.59 = load i8*, i8** %TP_0.59.in, align 8
  %t11208 = add i32 %TW32_0.35, 1
  %cond15 = icmp eq i8* %TP_0.59, inttoptr (i64 1 to i8*)
  br i1 %cond15, label %L_1115, label %L_1112

L_1738:                                           ; preds = %L_1108
  %t11229 = ptrtoint i8* %TP_1.19 to i64
  %cond14 = icmp eq i8* %TP_1.19, inttoptr (i64 1 to i8*)
  br i1 %cond14, label %L_1529, label %L_1112

L_1109:                                           ; preds = %L_1108
  %t11234 = shl nsw i64 %TW64_0.18, 3
  %t11235 = getelementptr inbounds i8, i8* %TP_0.60, i64 %t11234
  %t11237 = bitcast i8* %t11235 to i8**
  %t11238 = load i8*, i8** %t11237, align 8
  %t11240 = getelementptr inbounds i8, i8* %frontier.121, i64 8
  %t11245 = bitcast i8* %frontier.121 to i64*
  store i64 87, i64* %t11245, align 4
  %t11247 = getelementptr inbounds i8, i8* %frontier.121, i64 24
  %t11250 = bitcast i8* %t11240 to i8**
  store i8* %t11238, i8** %t11250, align 8
  %t11253 = getelementptr inbounds i8, i8* %frontier.121, i64 16
  br label %loop_58

L_1108:                                           ; preds = %loop_58, %L_1794
  %TP_1.19 = phi i8* [ %t5697, %L_1794 ], [ %TP_0.61, %loop_58 ]
  %TP_0.60 = phi i8* [ %t5701, %L_1794 ], [ %TP_1.20, %loop_58 ]
  %TW64_0.18 = phi i64 [ %t5693, %L_1794 ], [ %TW64_0.19, %loop_58 ]
  %stackTop.122 = phi i8* [ %t5689, %L_1794 ], [ %stackTop.123, %loop_58 ]
  %frontier.121 = phi i8* [ %t5753, %L_1794 ], [ %frontier.122, %loop_58 ]
  %trunc1959 = icmp sgt i64 %TW64_0.18, -1
  br i1 %trunc1959, label %L_1109, label %L_1738

loop_58:                                          ; preds = %L_1736, %L_1109
  %t11293.sink = phi i8* [ %t11293, %L_1736 ], [ %t11253, %L_1109 ]
  %t11281.sink = phi i8* [ %t11281, %L_1736 ], [ %TP_1.19, %L_1109 ]
  %TP_1.20 = phi i8* [ %TP_0.62.lcssa, %L_1736 ], [ %TP_0.60, %L_1109 ]
  %TP_0.61 = phi i8* [ inttoptr (i64 1 to i8*), %L_1736 ], [ %t11240, %L_1109 ]
  %TW64_0.19.in = phi i64 [ %t11305.lcssa, %L_1736 ], [ %TW64_0.18, %L_1109 ]
  %stackTop.123 = phi i8* [ %stackTop.124.lcssa, %L_1736 ], [ %stackTop.122, %L_1109 ]
  %frontier.122 = phi i8* [ %frontier.123.lcssa, %L_1736 ], [ %t11247, %L_1109 ]
  %t11294 = bitcast i8* %t11293.sink to i8**
  store i8* %t11281.sink, i8** %t11294, align 8
  %TW64_0.19 = add i64 %TW64_0.19.in, -1
  %t11268 = load i8*, i8** %t14203, align 8
  %t11270.not = icmp ult i8* %t11268, %frontier.122
  br i1 %t11270.not, label %L_1794, label %L_1108

L_1736.loopexit:                                  ; preds = %L_1536
  %.pre4496 = bitcast i8* %t5544 to i8**
  br label %L_1736

L_1736:                                           ; preds = %L_1736.loopexit, %L_1103
  %t11290.pre-phi = phi i8** [ %.pre4496, %L_1736.loopexit ], [ %t11318, %L_1103 ]
  %TP_0.62.lcssa = phi i8* [ %TP_1.44555, %L_1736.loopexit ], [ %t11346, %L_1103 ]
  %stackTop.124.lcssa = phi i8* [ %stackTop.604557, %L_1736.loopexit ], [ %t11310, %L_1103 ]
  %frontier.123.lcssa = phi i8* [ %frontier.624558, %L_1736.loopexit ], [ %t11349, %L_1103 ]
  %t11305.lcssa = phi i64 [ %t11305, %L_1736.loopexit ], [ %t113053506, %L_1103 ]
  %t11275 = getelementptr inbounds i8, i8* %stackTop.124.lcssa, i64 136
  %t11276 = bitcast i8* %t11275 to i8**
  %t11277 = load i8*, i8** %t11276, align 8
  %t11279 = getelementptr inbounds i8, i8* %stackTop.124.lcssa, i64 144
  %t11280 = bitcast i8* %t11279 to i8**
  %t11281 = load i8*, i8** %t11280, align 8
  store i8* %t11277, i8** %t11290.pre-phi, align 8
  %t11293 = getelementptr inbounds i8, i8* %stackTop.124.lcssa, i64 80
  br label %loop_58

L_1103:                                           ; preds = %L_1735
  %t11336 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t11337 = bitcast i8* %t11336 to i64*
  store i64 98, i64* %t11337, align 4
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %stackTop.0, i8** %t15200, align 8
  %t11346 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t11371, i64 75)
  %t11349 = load i8*, i8** %t15197, align 8
  %t11352 = load i8*, i8** %t15200, align 8
  %t11310 = getelementptr inbounds i8, i8* %t11352, i64 -176
  %t11314 = getelementptr inbounds i8, i8* %t11352, i64 -40
  %t11315 = bitcast i8* %t11314 to i8**
  %t11317 = getelementptr inbounds i8, i8* %t11352, i64 -112
  %t11318 = bitcast i8* %t11317 to i8**
  %t11319 = load i8*, i8** %t11318, align 8
  store i8* %t11319, i8** %t11315, align 8
  %t11321 = getelementptr inbounds i8, i8* %t11352, i64 -32
  %t11322 = bitcast i8* %t11321 to i8**
  %t11324 = getelementptr inbounds i8, i8* %t11352, i64 -96
  %t11325 = bitcast i8* %t11324 to i8**
  %t11326 = load i8*, i8** %t11325, align 8
  store i8* %t11326, i8** %t11322, align 8
  %t11329 = bitcast i8* %t11317 to i64*
  store i64 0, i64* %t11329, align 4
  %t113033504 = getelementptr inbounds i8, i8* %t11352, i64 -80
  %t113043505 = bitcast i8* %t113033504 to i64*
  %t113053506 = load i64, i64* %t113043505, align 4
  %t11306.not3507 = icmp sgt i64 %t113053506, 0
  br i1 %t11306.not3507, label %L_1531, label %L_1736

L_1735:                                           ; preds = %doSwitchNextBlock
  %t11356 = getelementptr inbounds i8, i8* %stackTop.0, i64 -88
  %t11357 = bitcast i8* %t11356 to i64*
  %t11363 = load i64, i64* @CommandLine_argv, align 4
  store i64 %t11363, i64* %t11357, align 4
  %t11369 = load i32, i32* @CommandLine_argc, align 4
  %t11371 = sext i32 %t11369 to i64
  %t11373 = getelementptr inbounds i8, i8* %stackTop.0, i64 -80
  %t11374 = bitcast i8* %t11373 to i64*
  store i64 %t11371, i64* %t11374, align 4
  %trunc1953 = icmp sgt i32 %t11369, -1
  br i1 %trunc1953, label %L_1103, label %L_1539

L_1734:                                           ; preds = %loop_56
  store i8* %t11492, i8** %t11491, align 8
  store i8* %t11496, i8** %t11495, align 8
  %t11391 = getelementptr inbounds i8, i8* %t11522, i64 -168
  %t11392 = bitcast i8* %t11391 to i64*
  store i64 61, i64* %t11392, align 4
  %t11395 = bitcast i8* %t11522 to i8**
  store i8* inttoptr (i64 4294967295 to i8*), i8** %t11395, align 8
  %t11398 = getelementptr inbounds i8, i8* %t11522, i64 8
  %t11399 = bitcast i8* %t11398 to i8**
  %t11401 = getelementptr inbounds i8, i8* %t11522, i64 -136
  %t11402 = bitcast i8* %t11401 to i8**
  %t11403 = load i8*, i8** %t11402, align 8
  store i8* %t11403, i8** %t11399, align 8
  %t11406 = getelementptr inbounds i8, i8* %t11522, i64 16
  %t11407 = bitcast i8* %t11406 to i64*
  store i64 8, i64* %t11407, align 4
  %t11409 = getelementptr inbounds i8, i8* %t11522, i64 24
  %t11410 = bitcast i8* %t11409 to i8**
  %t11412 = getelementptr inbounds i8, i8* %t11522, i64 -72
  %t11413 = bitcast i8* %t11412 to i8**
  %t11414 = load i8*, i8** %t11413, align 8
  store i8* %t11414, i8** %t11410, align 8
  %t11416 = getelementptr inbounds i8, i8* %t11522, i64 32
  %t11417 = bitcast i8* %t11416 to i8**
  store i8* %t11492, i8** %t11417, align 8
  %t11424 = getelementptr inbounds i8, i8* %t11522, i64 -8
  br label %L_213.sink.split

loop_56:                                          ; preds = %L_1540
  %t5454 = add nuw nsw i64 %TW64_0.203497, 1
  %t11429 = icmp ugt i64 %TW64_0.203497, 254
  br i1 %t11429, label %L_1734, label %L_1540

L_1097:                                           ; preds = %L_1731, %L_1097
  %TW64_0.213496 = phi i64 [ %t11457, %L_1097 ], [ 0, %L_1731 ]
  %t11462 = getelementptr inbounds i8, i8* getelementptr (i8, i8* @staticHeapI, i64 3312), i64 %TW64_0.213496
  %t11465 = load i8, i8* %t11462, align 1
  %t11469 = zext i8 %t11465 to i64
  %t11467 = trunc i64 %TW64_0.213496 to i8
  %t11457 = add nuw nsw i64 %TW64_0.213496, 1
  %t11439 = getelementptr inbounds i8, i8* %t11516, i64 %t11469
  store i8 %t11467, i8* %t11439, align 1
  %t11475 = icmp ult i64 %TW64_0.213496, 63
  br i1 %t11475, label %L_1097, label %L_1540

L_1731:                                           ; preds = %loop_53
  store i8* %t11607, i8** %t11606, align 8
  store i8* %t11611, i8** %t11610, align 8
  %t11506 = getelementptr inbounds i8, i8* %t11637, i64 -8
  %t11507 = bitcast i8* %t11506 to i64*
  store i64 98, i64* %t11507, align 4
  store i8* %t11634, i8** %t15197, align 8
  store i8* %t11637, i8** %t15200, align 8
  %t11516 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 256, i64 21)
  %t11519 = load i8*, i8** %t15197, align 8
  %t11522 = load i8*, i8** %t15200, align 8
  %t11490 = getelementptr inbounds i8, i8* %t11522, i64 -112
  %t11491 = bitcast i8* %t11490 to i8**
  %t11492 = load i8*, i8** %t11491, align 8
  %t11494 = getelementptr inbounds i8, i8* %t11522, i64 -96
  %t11495 = bitcast i8* %t11494 to i8**
  %t11496 = load i8*, i8** %t11495, align 8
  call void @llvm.memset.p0i8.i64(i8* noundef nonnull align 1 dereferenceable(256) %t11516, i8 -1, i64 256, i1 false)
  br label %L_1097

loop_53:                                          ; preds = %L_1544
  %t5422 = add nuw nsw i64 %TW64_0.233494, 1
  %t11524 = icmp ugt i64 %TW64_0.233494, 254
  br i1 %t11524, label %L_1731, label %L_1544

L_1090:                                           ; preds = %L_1728, %L_1090
  %TW64_0.243493 = phi i64 [ 0, %L_1728 ], [ %t11552, %L_1090 ]
  %t11557 = getelementptr inbounds i8, i8* getelementptr (i8, i8* @staticHeapI, i64 3312), i64 %TW64_0.243493
  %t11560 = load i8, i8* %t11557, align 1
  %t11564 = zext i8 %t11560 to i64
  %t11562 = trunc i64 %TW64_0.243493 to i8
  %t11552 = add nuw nsw i64 %TW64_0.243493, 1
  %t11534 = getelementptr inbounds i8, i8* %t11631, i64 %t11564
  store i8 %t11562, i8* %t11534, align 1
  %t11570 = icmp ult i64 %TW64_0.243493, 63
  br i1 %t11570, label %L_1090, label %L_1544

L_1728:                                           ; preds = %loop_49.backedge
  store i8* %t12344, i8** %t12343, align 8
  store i8* %t12348, i8** %t12347, align 8
  %t11621 = getelementptr inbounds i8, i8* %t12374, i64 -8
  %t11622 = bitcast i8* %t11621 to i64*
  store i64 98, i64* %t11622, align 4
  store i8* %t12371, i8** %t15197, align 8
  store i8* %t12374, i8** %t15200, align 8
  %t11631 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 256, i64 21)
  %t11634 = load i8*, i8** %t15197, align 8
  %t11637 = load i8*, i8** %t15200, align 8
  %t11605 = getelementptr inbounds i8, i8* %t11637, i64 -112
  %t11606 = bitcast i8* %t11605 to i8**
  %t11607 = load i8*, i8** %t11606, align 8
  %t11609 = getelementptr inbounds i8, i8* %t11637, i64 -96
  %t11610 = bitcast i8* %t11609 to i8**
  %t11611 = load i8*, i8** %t11610, align 8
  call void @llvm.memset.p0i8.i64(i8* noundef nonnull align 1 dereferenceable(256) %t11631, i8 -1, i64 256, i1 false)
  %t11577 = getelementptr inbounds i8, i8* %t11631, i64 9
  store i8 65, i8* %t11577, align 1
  %t11582 = getelementptr inbounds i8, i8* %t11631, i64 10
  store i8 65, i8* %t11582, align 1
  %t11587 = getelementptr inbounds i8, i8* %t11631, i64 13
  store i8 65, i8* %t11587, align 1
  %t11592 = getelementptr inbounds i8, i8* %t11631, i64 32
  store i8 65, i8* %t11592, align 1
  br label %L_1090

L_1084:                                           ; preds = %L_1079
  %t12339.le3658 = getelementptr inbounds i8, i8* %t12374, i64 -176
  %t11663 = getelementptr inbounds i8, i8* %t12374, i64 -152
  %t11664 = bitcast i8* %t11663 to i8**
  %t11666 = getelementptr inbounds i8, i8* %t12374, i64 -16
  %t11667 = bitcast i8* %t11666 to i8**
  %t11668 = load i8*, i8** %t11667, align 8
  store i8* %t11668, i8** %t11664, align 8
  %t11670 = getelementptr inbounds i8, i8* %t12374, i64 -120
  br label %L_981

L_1082:                                           ; preds = %L_1074
  %t116964656 = zext i32 %TW32_0.3634905725 to i64
  %t11678 = getelementptr inbounds i8, i8* %t12368, i64 %t116964656
  br label %loop_49.backedge

loop_49.backedge:                                 ; preds = %L_1063, %L_1082, %L_1066, %L_1060
  %t11678.sink = phi i8* [ %t11678, %L_1082 ], [ %t11850, %L_1066 ], [ %t11880, %L_1060 ], [ %t11791, %L_1063 ]
  store i8 %t11940, i8* %t11678.sink, align 1
  %t11977 = add nsw i32 %t1197734915728, -1
  %t11973.not = icmp eq i32 %t11977, 0
  br i1 %t11973.not, label %L_1728, label %L_1053

L_1079:                                           ; preds = %L_1074
  %spec.select2473 = select i1 %t11742.not, i32 %t119145727, i32 %t119215726
  %t117224657 = zext i32 %TW32_0.3634905725 to i64
  %t11704 = getelementptr inbounds i8, i8* %t12368, i64 %t117224657
  store i8 %spec.select2472, i8* %t11704, align 1
  %t11928 = shl nuw nsw i32 %spec.select2473, 1
  %t11921 = or i32 %t11928, 1
  %t11914 = add i32 %t11928, 2
  %t11917.not = icmp eq i32 %t11921, 2147483647
  br i1 %t11917.not, label %L_1084, label %L_1056

L_1074:                                           ; preds = %L_1072
  %t117654655 = zext i32 %t119145727 to i64
  %t11736 = getelementptr inbounds i8, i8* %t12368, i64 %t117654655
  %t11739 = load i8, i8* %t11736, align 1
  %t11742.not = icmp ult i8 %t11763, %t11739
  %spec.select2472 = select i1 %t11742.not, i8 %t11739, i8 %t11763
  %t11729.not = icmp ult i8 %spec.select2472, %t11940
  br i1 %t11729.not, label %L_1082, label %L_1079

L_1073:                                           ; preds = %L_1072
  %t12339.le3646 = getelementptr inbounds i8, i8* %t12374, i64 -176
  %t11746 = getelementptr inbounds i8, i8* %t12374, i64 -152
  %t11747 = bitcast i8* %t11746 to i8**
  %t11749 = getelementptr inbounds i8, i8* %t12374, i64 -16
  %t11750 = bitcast i8* %t11749 to i8**
  %t11751 = load i8*, i8** %t11750, align 8
  store i8* %t11751, i8** %t11747, align 8
  %t11753 = getelementptr inbounds i8, i8* %t12374, i64 -120
  br label %L_981

L_1072:                                           ; preds = %L_1070
  %t117834654 = zext i32 %t119215726 to i64
  %t11760 = getelementptr inbounds i8, i8* %t12368, i64 %t117834654
  %t11763 = load i8, i8* %t11760, align 1
  %t11767 = icmp ugt i32 %t119145727, 63
  br i1 %t11767, label %L_1073, label %L_1074

L_1071:                                           ; preds = %L_1070
  %t12339.le3648 = getelementptr inbounds i8, i8* %t12374, i64 -176
  %t11771 = getelementptr inbounds i8, i8* %t12374, i64 -152
  %t11772 = bitcast i8* %t11771 to i8**
  %t11774 = getelementptr inbounds i8, i8* %t12374, i64 -16
  %t11775 = bitcast i8* %t11774 to i8**
  %t11776 = load i8*, i8** %t11775, align 8
  store i8* %t11776, i8** %t11772, align 8
  %t11778 = getelementptr inbounds i8, i8* %t12374, i64 -120
  br label %L_981

L_1070:                                           ; preds = %L_1056
  %t11785 = icmp ugt i32 %t119215726, 63
  br i1 %t11785, label %L_1071, label %L_1072

L_1066:                                           ; preds = %L_1063
  store i8 %t11853, i8* %t11791, align 1
  br label %loop_49.backedge

L_1063:                                           ; preds = %L_1061
  %t118724658 = zext i32 %t119215726 to i64
  %t11850 = getelementptr inbounds i8, i8* %t12368, i64 %t118724658
  %t11853 = load i8, i8* %t11850, align 1
  %t11856.not = icmp ult i8 %t11853, %t11940
  %t118094659 = zext i32 %TW32_0.3634905725 to i64
  %t11791 = getelementptr inbounds i8, i8* %t12368, i64 %t118094659
  br i1 %t11856.not, label %loop_49.backedge, label %L_1066

L_1062:                                           ; preds = %L_1061
  %t12339.le3654 = getelementptr inbounds i8, i8* %t12374, i64 -176
  %t11860 = getelementptr inbounds i8, i8* %t12374, i64 -152
  %t11861 = bitcast i8* %t11860 to i8**
  %t11863 = getelementptr inbounds i8, i8* %t12374, i64 -16
  %t11864 = bitcast i8* %t11863 to i8**
  %t11865 = load i8*, i8** %t11864, align 8
  store i8* %t11865, i8** %t11861, align 8
  %t11867 = getelementptr inbounds i8, i8* %t12374, i64 -120
  br label %L_981

L_1061:                                           ; preds = %L_1057
  %t11874 = icmp ugt i32 %t119215726, 63
  br i1 %t11874, label %L_1062, label %L_1063

L_1060:                                           ; preds = %L_1057
  %t118984661 = zext i32 %TW32_0.3634905725 to i64
  %t11880 = getelementptr inbounds i8, i8* %t12368, i64 %t118984661
  br label %loop_49.backedge

L_1057:                                           ; preds = %L_1056
  %t11905.not = icmp eq i32 %t119145727, %t1197734915728
  br i1 %t11905.not, label %L_1061, label %L_1060

L_1056:                                           ; preds = %L_1053, %L_1079
  %t119145727 = phi i32 [ 2, %L_1053 ], [ %t11914, %L_1079 ]
  %t119215726 = phi i32 [ 1, %L_1053 ], [ %t11921, %L_1079 ]
  %TW32_0.3634905725 = phi i32 [ 0, %L_1053 ], [ %spec.select2473, %L_1079 ]
  %t11910.not = icmp slt i32 %t119145727, %t1197734915728
  br i1 %t11910.not, label %L_1070, label %L_1057

L_1053:                                           ; preds = %loop_47.backedge, %loop_49.backedge
  %t1197734915728 = phi i32 [ %t11977, %loop_49.backedge ], [ 63, %loop_47.backedge ]
  %t11967 = sext i32 %t1197734915728 to i64
  %t11937 = getelementptr inbounds i8, i8* %t12368, i64 %t11967
  %t11940 = load i8, i8* %t11937, align 1
  %t11946 = load i8, i8* %t12368, align 1
  store i8 %t11946, i8* %t11937, align 1
  br label %L_1056

L_1045:                                           ; preds = %L_1015.preheader, %L_1040
  %t12339.le3678 = getelementptr inbounds i8, i8* %t12374, i64 -176
  %t12024 = getelementptr inbounds i8, i8* %t12374, i64 -152
  %t12025 = bitcast i8* %t12024 to i8**
  %t12027 = getelementptr inbounds i8, i8* %t12374, i64 -16
  %t12028 = bitcast i8* %t12027 to i8**
  %t12029 = load i8*, i8** %t12028, align 8
  store i8* %t12029, i8** %t12025, align 8
  %t12031 = getelementptr inbounds i8, i8* %t12374, i64 -120
  br label %L_981

L_1043:                                           ; preds = %L_1041
  %t120574666 = zext i32 %TW32_0.3834825719 to i64
  %t12039 = getelementptr inbounds i8, i8* %t12368, i64 %t120574666
  br label %loop_47.backedge

loop_47.backedge:                                 ; preds = %L_1043, %L_1030, %L_1027, %L_1021
  %t12039.sink = phi i8* [ %t12039, %L_1043 ], [ %t12152, %L_1030 ], [ %t12211, %L_1027 ], [ %t12241, %L_1021 ]
  store i8 %t12299, i8* %t12039.sink, align 1
  %t12327.not = icmp eq i32 %t12320, 0
  br i1 %t12327.not, label %L_1053, label %L_1015.preheader

L_1042:                                           ; preds = %L_1041
  %t12339.le3670 = getelementptr inbounds i8, i8* %t12374, i64 -176
  %t12045 = getelementptr inbounds i8, i8* %t12374, i64 -152
  %t12046 = bitcast i8* %t12045 to i8**
  %t12048 = getelementptr inbounds i8, i8* %t12374, i64 -16
  %t12049 = bitcast i8* %t12048 to i8**
  %t12050 = load i8*, i8** %t12049, align 8
  store i8* %t12050, i8** %t12046, align 8
  %t12052 = getelementptr inbounds i8, i8* %t12374, i64 -120
  br label %L_981

L_1041:                                           ; preds = %L_1035
  br i1 %t12059, label %L_1042, label %L_1043

L_1040:                                           ; preds = %L_1038
  %t120834667 = zext i32 %TW32_0.3834825719 to i64
  %t12065 = getelementptr inbounds i8, i8* %t12368, i64 %t120834667
  store i8 %spec.select2492, i8* %t12065, align 1
  %t12287 = shl i32 %spec.select2493, 1
  %t12280 = or i32 %t12287, 1
  %t12276.not = icmp eq i32 %t12280, 2147483647
  br i1 %t12276.not, label %L_1045, label %L_1017

L_1039:                                           ; preds = %L_1038
  %t12339.le3672 = getelementptr inbounds i8, i8* %t12374, i64 -176
  %t12071 = getelementptr inbounds i8, i8* %t12374, i64 -152
  %t12072 = bitcast i8* %t12071 to i8**
  %t12074 = getelementptr inbounds i8, i8* %t12374, i64 -16
  %t12075 = bitcast i8* %t12074 to i8**
  %t12076 = load i8*, i8** %t12075, align 8
  store i8* %t12076, i8** %t12072, align 8
  %t12078 = getelementptr inbounds i8, i8* %t12374, i64 -120
  br label %L_981

L_1038:                                           ; preds = %L_1035
  br i1 %t12059, label %L_1039, label %L_1040

L_1035:                                           ; preds = %L_1033
  %t121264665 = zext i32 %t122735721 to i64
  %t12097 = getelementptr inbounds i8, i8* %t12368, i64 %t121264665
  %t12100 = load i8, i8* %t12097, align 1
  %t12103.not = icmp ult i8 %t12124, %t12100
  %spec.select2492 = select i1 %t12103.not, i8 %t12100, i8 %t12124
  %spec.select2493 = select i1 %t12103.not, i32 %t122735721, i32 %t122805720
  %t12090.not = icmp ult i8 %spec.select2492, %t12299
  %t12059 = icmp ugt i32 %TW32_0.3834825719, 63
  br i1 %t12090.not, label %L_1041, label %L_1038

L_1034:                                           ; preds = %L_1033
  %t12339.le3674 = getelementptr inbounds i8, i8* %t12374, i64 -176
  %t12107 = getelementptr inbounds i8, i8* %t12374, i64 -152
  %t12108 = bitcast i8* %t12107 to i8**
  %t12110 = getelementptr inbounds i8, i8* %t12374, i64 -16
  %t12111 = bitcast i8* %t12110 to i8**
  %t12112 = load i8*, i8** %t12111, align 8
  store i8* %t12112, i8** %t12108, align 8
  %t12114 = getelementptr inbounds i8, i8* %t12374, i64 -120
  br label %L_981

L_1033:                                           ; preds = %L_1031
  %t121444664 = zext i32 %t122805720 to i64
  %t12121 = getelementptr inbounds i8, i8* %t12368, i64 %t121444664
  %t12124 = load i8, i8* %t12121, align 1
  %t12128 = icmp ugt i32 %t122735721, 63
  br i1 %t12128, label %L_1034, label %L_1035

L_1032:                                           ; preds = %L_1031
  %t12339.le3676 = getelementptr inbounds i8, i8* %t12374, i64 -176
  %t12132 = getelementptr inbounds i8, i8* %t12374, i64 -152
  %t12133 = bitcast i8* %t12132 to i8**
  %t12135 = getelementptr inbounds i8, i8* %t12374, i64 -16
  %t12136 = bitcast i8* %t12135 to i8**
  %t12137 = load i8*, i8** %t12136, align 8
  store i8* %t12137, i8** %t12133, align 8
  %t12139 = getelementptr inbounds i8, i8* %t12374, i64 -120
  br label %L_981

L_1031:                                           ; preds = %L_1017
  %t12146 = icmp ugt i32 %t122805720, 63
  br i1 %t12146, label %L_1032, label %L_1033

L_1030:                                           ; preds = %L_1028
  %t121704653 = zext i32 %TW32_0.3834825719 to i64
  %t12152 = getelementptr inbounds i8, i8* %t12368, i64 %t121704653
  br label %loop_47.backedge

L_1029:                                           ; preds = %L_1028
  %t12339.le3662 = getelementptr inbounds i8, i8* %t12374, i64 -176
  %t12158 = getelementptr inbounds i8, i8* %t12374, i64 -152
  %t12159 = bitcast i8* %t12158 to i8**
  %t12161 = getelementptr inbounds i8, i8* %t12374, i64 -16
  %t12162 = bitcast i8* %t12161 to i8**
  %t12163 = load i8*, i8** %t12162, align 8
  store i8* %t12163, i8** %t12159, align 8
  %t12165 = getelementptr inbounds i8, i8* %t12374, i64 -120
  br label %L_981

L_1028:                                           ; preds = %L_1024
  br i1 %t12172, label %L_1029, label %L_1030

L_1027:                                           ; preds = %L_1025
  %t122034662 = zext i32 %TW32_0.3834825719 to i64
  %t12178 = getelementptr inbounds i8, i8* %t12368, i64 %t122034662
  store i8 %t12214, i8* %t12178, align 1
  br label %loop_47.backedge

L_1026:                                           ; preds = %L_1025
  %t12339.le3664 = getelementptr inbounds i8, i8* %t12374, i64 -176
  %t12191 = getelementptr inbounds i8, i8* %t12374, i64 -152
  %t12192 = bitcast i8* %t12191 to i8**
  %t12194 = getelementptr inbounds i8, i8* %t12374, i64 -16
  %t12195 = bitcast i8* %t12194 to i8**
  %t12196 = load i8*, i8** %t12195, align 8
  store i8* %t12196, i8** %t12192, align 8
  %t12198 = getelementptr inbounds i8, i8* %t12374, i64 -120
  br label %L_981

L_1025:                                           ; preds = %L_1024
  br i1 %t12172, label %L_1026, label %L_1027

L_1024:                                           ; preds = %L_1018
  %t122334652 = zext i32 %t122805720 to i64
  %t12211 = getelementptr inbounds i8, i8* %t12368, i64 %t122334652
  %t12214 = load i8, i8* %t12211, align 1
  %t12217.not = icmp ult i8 %t12214, %t12299
  %t12172 = icmp ugt i32 %TW32_0.3834825719, 63
  br i1 %t12217.not, label %L_1028, label %L_1025

L_1021:                                           ; preds = %L_1019
  %t122594663 = zext i32 %TW32_0.3834825719 to i64
  %t12241 = getelementptr inbounds i8, i8* %t12368, i64 %t122594663
  br label %loop_47.backedge

L_1020:                                           ; preds = %L_1019
  %t12339.le3668 = getelementptr inbounds i8, i8* %t12374, i64 -176
  %t12247 = getelementptr inbounds i8, i8* %t12374, i64 -152
  %t12248 = bitcast i8* %t12247 to i8**
  %t12250 = getelementptr inbounds i8, i8* %t12374, i64 -16
  %t12251 = bitcast i8* %t12250 to i8**
  %t12252 = load i8*, i8** %t12251, align 8
  store i8* %t12252, i8** %t12248, align 8
  %t12254 = getelementptr inbounds i8, i8* %t12374, i64 -120
  br label %L_981

L_1019:                                           ; preds = %L_1018
  %t12261 = icmp ugt i32 %TW32_0.3834825719, 63
  br i1 %t12261, label %L_1020, label %L_1021

L_1018:                                           ; preds = %L_1017
  %t12265.not = icmp eq i32 %t122735721, 64
  br i1 %t12265.not, label %L_1024, label %L_1019

L_1017:                                           ; preds = %L_1015.preheader, %L_1040
  %t122735721.in = phi i32 [ %t12287, %L_1040 ], [ %t122875715, %L_1015.preheader ]
  %t122805720 = phi i32 [ %t12280, %L_1040 ], [ %t122805716, %L_1015.preheader ]
  %TW32_0.3834825719 = phi i32 [ %spec.select2493, %L_1040 ], [ %t12320, %L_1015.preheader ]
  %t122735721 = add i32 %t122735721.in, 2
  %t12269 = icmp sgt i32 %t122735721, 63
  br i1 %t12269, label %L_1018, label %L_1031

L_1015.preheader:                                 ; preds = %L_1015.preheader.preheader, %loop_47.backedge
  %TW32_0.393489 = phi i32 [ %t12320, %loop_47.backedge ], [ 32, %L_1015.preheader.preheader ]
  %t12320 = add nsw i32 %TW32_0.393489, -1
  %t12314 = sext i32 %t12320 to i64
  %t12296 = getelementptr inbounds i8, i8* %t12368, i64 %t12314
  %t12299 = load i8, i8* %t12296, align 1
  %t122875715 = shl i32 %t12320, 1
  %t122805716 = or i32 %t122875715, 1
  %t12276.not5718 = icmp eq i32 %t122805716, 2147483647
  br i1 %t12276.not5718, label %L_1045, label %L_1017

loop_44.preheader:                                ; preds = %L_1555, %L_1003
  %t12351 = bitcast i8* %t12409 to i8**
  store i8* %t12415, i8** %t12351, align 8
  %t12354 = getelementptr inbounds i8, i8* %t12445, i64 -96
  %t12355 = bitcast i8* %t12354 to i8**
  store i8* %t12419, i8** %t12355, align 8
  %t12358 = getelementptr inbounds i8, i8* %t12445, i64 -8
  %t12359 = bitcast i8* %t12358 to i64*
  store i64 98, i64* %t12359, align 4
  store i8* %t12442, i8** %t15197, align 8
  store i8* %t12445, i8** %t15200, align 8
  %t12368 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 64, i64 21)
  %t12371 = load i8*, i8** %t15197, align 8
  %t12374 = load i8*, i8** %t15200, align 8
  %t12342 = getelementptr inbounds i8, i8* %t12374, i64 -112
  %t12343 = bitcast i8* %t12342 to i8**
  %t12344 = load i8*, i8** %t12343, align 8
  %t12346 = getelementptr inbounds i8, i8* %t12374, i64 -96
  %t12347 = bitcast i8* %t12346 to i8**
  %t12348 = load i8*, i8** %t12347, align 8
  %scevgep = getelementptr i8, i8* %t12368, i64 64
  %bound0 = icmp ult i8* %t12368, getelementptr (i8, i8* @staticHeapI, i64 3376)
  %bound1 = icmp ugt i8* %scevgep, getelementptr (i8, i8* @staticHeapI, i64 3312)
  %found.conflict = and i1 %bound0, %bound1
  br i1 %found.conflict, label %L_1551, label %vector.body

vector.body:                                      ; preds = %loop_44.preheader
  %wide.load = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3312) to <4 x i8>*), align 1, !alias.scope !37
  %99 = bitcast i8* %t12368 to <4 x i8>*
  store <4 x i8> %wide.load, <4 x i8>* %99, align 1, !alias.scope !40, !noalias !37
  %wide.load.1 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3316) to <4 x i8>*), align 1, !alias.scope !37
  %100 = getelementptr inbounds i8, i8* %t12368, i64 4
  %101 = bitcast i8* %100 to <4 x i8>*
  store <4 x i8> %wide.load.1, <4 x i8>* %101, align 1, !alias.scope !40, !noalias !37
  %wide.load.2 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3320) to <4 x i8>*), align 1, !alias.scope !37
  %102 = getelementptr inbounds i8, i8* %t12368, i64 8
  %103 = bitcast i8* %102 to <4 x i8>*
  store <4 x i8> %wide.load.2, <4 x i8>* %103, align 1, !alias.scope !40, !noalias !37
  %wide.load.3 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3324) to <4 x i8>*), align 1, !alias.scope !37
  %104 = getelementptr inbounds i8, i8* %t12368, i64 12
  %105 = bitcast i8* %104 to <4 x i8>*
  store <4 x i8> %wide.load.3, <4 x i8>* %105, align 1, !alias.scope !40, !noalias !37
  %wide.load.4 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3328) to <4 x i8>*), align 1, !alias.scope !37
  %106 = getelementptr inbounds i8, i8* %t12368, i64 16
  %107 = bitcast i8* %106 to <4 x i8>*
  store <4 x i8> %wide.load.4, <4 x i8>* %107, align 1, !alias.scope !40, !noalias !37
  %wide.load.5 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3332) to <4 x i8>*), align 1, !alias.scope !37
  %108 = getelementptr inbounds i8, i8* %t12368, i64 20
  %109 = bitcast i8* %108 to <4 x i8>*
  store <4 x i8> %wide.load.5, <4 x i8>* %109, align 1, !alias.scope !40, !noalias !37
  %wide.load.6 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3336) to <4 x i8>*), align 1, !alias.scope !37
  %110 = getelementptr inbounds i8, i8* %t12368, i64 24
  %111 = bitcast i8* %110 to <4 x i8>*
  store <4 x i8> %wide.load.6, <4 x i8>* %111, align 1, !alias.scope !40, !noalias !37
  %wide.load.7 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3340) to <4 x i8>*), align 1, !alias.scope !37
  %112 = getelementptr inbounds i8, i8* %t12368, i64 28
  %113 = bitcast i8* %112 to <4 x i8>*
  store <4 x i8> %wide.load.7, <4 x i8>* %113, align 1, !alias.scope !40, !noalias !37
  %wide.load.8 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3344) to <4 x i8>*), align 1, !alias.scope !37
  %114 = getelementptr inbounds i8, i8* %t12368, i64 32
  %115 = bitcast i8* %114 to <4 x i8>*
  store <4 x i8> %wide.load.8, <4 x i8>* %115, align 1, !alias.scope !40, !noalias !37
  %wide.load.9 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3348) to <4 x i8>*), align 1, !alias.scope !37
  %116 = getelementptr inbounds i8, i8* %t12368, i64 36
  %117 = bitcast i8* %116 to <4 x i8>*
  store <4 x i8> %wide.load.9, <4 x i8>* %117, align 1, !alias.scope !40, !noalias !37
  %wide.load.10 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3352) to <4 x i8>*), align 1, !alias.scope !37
  %118 = getelementptr inbounds i8, i8* %t12368, i64 40
  %119 = bitcast i8* %118 to <4 x i8>*
  store <4 x i8> %wide.load.10, <4 x i8>* %119, align 1, !alias.scope !40, !noalias !37
  %wide.load.11 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3356) to <4 x i8>*), align 1, !alias.scope !37
  %120 = getelementptr inbounds i8, i8* %t12368, i64 44
  %121 = bitcast i8* %120 to <4 x i8>*
  store <4 x i8> %wide.load.11, <4 x i8>* %121, align 1, !alias.scope !40, !noalias !37
  %wide.load.12 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3360) to <4 x i8>*), align 1, !alias.scope !37
  %122 = getelementptr inbounds i8, i8* %t12368, i64 48
  %123 = bitcast i8* %122 to <4 x i8>*
  store <4 x i8> %wide.load.12, <4 x i8>* %123, align 1, !alias.scope !40, !noalias !37
  %wide.load.13 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3364) to <4 x i8>*), align 1, !alias.scope !37
  %124 = getelementptr inbounds i8, i8* %t12368, i64 52
  %125 = bitcast i8* %124 to <4 x i8>*
  store <4 x i8> %wide.load.13, <4 x i8>* %125, align 1, !alias.scope !40, !noalias !37
  %wide.load.14 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3368) to <4 x i8>*), align 1, !alias.scope !37
  %126 = getelementptr inbounds i8, i8* %t12368, i64 56
  %127 = bitcast i8* %126 to <4 x i8>*
  store <4 x i8> %wide.load.14, <4 x i8>* %127, align 1, !alias.scope !40, !noalias !37
  %wide.load.15 = load <4 x i8>, <4 x i8>* bitcast (i8* getelementptr (i8, i8* @staticHeapI, i64 3372) to <4 x i8>*), align 1, !alias.scope !37
  %128 = getelementptr inbounds i8, i8* %t12368, i64 60
  %129 = bitcast i8* %128 to <4 x i8>*
  store <4 x i8> %wide.load.15, <4 x i8>* %129, align 1, !alias.scope !40, !noalias !37
  br label %L_1015.preheader.preheader

L_1015.preheader.preheader:                       ; preds = %L_1551, %vector.body
  br label %L_1015.preheader

L_1003:                                           ; preds = %L_1002.thread, %L_1002
  %t124472634 = phi i64 [ 0, %L_1002.thread ], [ %t12447, %L_1002 ]
  %t12429 = getelementptr inbounds i8, i8* %stackTop.125, i64 168
  %t12430 = bitcast i8* %t12429 to i64*
  store i64 97, i64* %t12430, align 4
  %t12432 = getelementptr inbounds i8, i8* %stackTop.125, i64 176
  store i8* %t12567, i8** %t15197, align 8
  store i8* %t12432, i8** %t15200, align 8
  %t12439 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 %t124472634, i64 73)
  %t12442 = load i8*, i8** %t15197, align 8
  %t12445 = load i8*, i8** %t15200, align 8
  %t12409 = getelementptr inbounds i8, i8* %t12445, i64 -112
  %t12410 = bitcast i8* %t12409 to i64*
  %t12411 = load i64, i64* %t12410, align 4
  %t12413 = getelementptr inbounds i8, i8* %t12445, i64 -80
  %t12414 = bitcast i8* %t12413 to i8**
  %t12415 = load i8*, i8** %t12414, align 8
  %t12417 = getelementptr inbounds i8, i8* %t12445, i64 -40
  %t12418 = bitcast i8* %t12417 to i8**
  %t12419 = load i8*, i8** %t12418, align 8
  %t12402.not3477 = icmp sgt i64 %t12411, 0
  br i1 %t12402.not3477, label %L_1554.preheader, label %loop_44.preheader

L_1554.preheader:                                 ; preds = %L_1003
  %t12421 = getelementptr inbounds i8, i8* %t12445, i64 -88
  %t12422 = bitcast i8* %t12421 to i8**
  %t12423 = load i8*, i8** %t12422, align 8
  br label %L_1554

L_1002:                                           ; preds = %L_999
  %t12447 = sext i32 %t12468 to i64
  %t12450 = bitcast i8* %t12490 to i64*
  store i64 %t12447, i64* %t12450, align 4
  %trunc1864 = icmp sgt i32 %t12468, -1
  br i1 %trunc1864, label %L_1003, label %L_1557

L_999:                                            ; preds = %L_1721, %L_999
  %t12503.pn = phi i8* [ %TP_0.63, %L_999 ], [ %t12560, %L_1721 ]
  %TW32_0.42 = phi i32 [ %t12468, %L_999 ], [ 0, %L_1721 ]
  %TP_0.63.in.in = getelementptr inbounds i8, i8* %t12503.pn, i64 8
  %TP_0.63.in = bitcast i8* %TP_0.63.in.in to i8**
  %TP_0.63 = load i8*, i8** %TP_0.63.in, align 8
  %t12468 = add i32 %TW32_0.42, 1
  %cond11 = icmp eq i8* %TP_0.63, inttoptr (i64 1 to i8*)
  br i1 %cond11, label %L_1002, label %L_999

L_1721:                                           ; preds = %L_995
  %t12747.lcssa = getelementptr inbounds i8, i8* %stackTop.125, i64 96
  %t12488 = bitcast i8* %t12747.lcssa to i8**
  %t12490 = getelementptr inbounds i8, i8* %stackTop.125, i64 64
  %t12491 = bitcast i8* %t12490 to i8**
  %t12492 = load i8*, i8** %t12491, align 8
  store i8* %t12492, i8** %t12488, align 8
  %t12494 = getelementptr inbounds i8, i8* %stackTop.125, i64 136
  %t12495 = bitcast i8* %t12494 to i8**
  %t12497 = getelementptr inbounds i8, i8* %stackTop.125, i64 80
  %t12498 = bitcast i8* %t12497 to i8**
  %t12499 = load i8*, i8** %t12498, align 8
  store i8* %t12499, i8** %t12495, align 8
  %cond10 = icmp eq i8* %t12560, inttoptr (i64 1 to i8*)
  br i1 %cond10, label %L_1002.thread, label %L_999

L_1002.thread:                                    ; preds = %L_1721
  %t124502631 = bitcast i8* %t12490 to i64*
  store i64 0, i64* %t124502631, align 4
  br label %L_1003

L_1720:                                           ; preds = %L_991
  %t12516 = getelementptr inbounds i8, i8* %stackTop.1273472, i64 144
  %t12517 = bitcast i8* %t12516 to i8**
  %t12519 = getelementptr inbounds i8, i8* %stackTop.1273472, i64 64
  %t12520 = bitcast i8* %t12519 to i8**
  %t12521 = load i8*, i8** %t12520, align 8
  store i8* %t12521, i8** %t12517, align 8
  %t12523 = getelementptr inbounds i8, i8* %stackTop.1273472, i64 152
  %t12524 = bitcast i8* %t12523 to i8**
  %t12526 = getelementptr inbounds i8, i8* %stackTop.1273472, i64 80
  %t12527 = bitcast i8* %t12526 to i8**
  %t12528 = load i8*, i8** %t12527, align 8
  store i8* %t12528, i8** %t12524, align 8
  %t12530 = getelementptr inbounds i8, i8* %stackTop.1273472, i64 168
  %t12531 = bitcast i8* %t12530 to i64*
  store i64 96, i64* %t12531, align 4
  %t12533 = getelementptr inbounds i8, i8* %stackTop.1273472, i64 176
  store i8* %frontier.1263473, i8** %t15197, align 8
  store i8* %t12533, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t12543 = load i8*, i8** %t15197, align 8
  %t12546 = load i8*, i8** %t15200, align 8
  %t12506 = getelementptr inbounds i8, i8* %t12546, i64 -176
  br label %L_994

L_996:                                            ; preds = %L_994
  %t12548 = getelementptr inbounds i8, i8* %stackTop.125, i64 24
  %t12549 = bitcast i8* %t12548 to i8**
  %t12551 = getelementptr inbounds i8, i8* %stackTop.125, i64 160
  %t12552 = bitcast i8* %t12551 to i8**
  %t12553 = load i8*, i8** %t12552, align 8
  store i8* %t12553, i8** %t12549, align 8
  %t12555 = getelementptr inbounds i8, i8* %stackTop.125, i64 56
  br label %L_981

L_995:                                            ; preds = %L_994
  %t12602 = add nsw i32 %t12601, -1
  %t12560 = getelementptr inbounds i8, i8* %frontier.124, i64 8
  %t12565 = bitcast i8* %frontier.124 to i64*
  store i64 117, i64* %t12565, align 4
  %t12567 = getelementptr inbounds i8, i8* %frontier.124, i64 24
  %t12570 = bitcast i8* %t12560 to i32*
  %t12572 = getelementptr inbounds i8, i8* %stackTop.125, i64 140
  %t12573 = bitcast i8* %t12572 to i32*
  %t12574 = load i32, i32* %t12573, align 4
  store i32 %t12574, i32* %t12570, align 4
  %t12576 = getelementptr inbounds i8, i8* %frontier.124, i64 16
  %t12577 = bitcast i8* %t12576 to i8**
  %t12579 = getelementptr inbounds i8, i8* %stackTop.125, i64 88
  %t12580 = bitcast i8* %t12579 to i8**
  %t12581 = load i8*, i8** %t12580, align 8
  store i8* %t12581, i8** %t12577, align 8
  %t12583 = getelementptr inbounds i8, i8* %stackTop.125, i64 136
  %t12584 = bitcast i8* %t12583 to i32*
  %t12585 = load i32, i32* %t12584, align 4
  store i8* %t12560, i8** %t12580, align 8
  store i32 %t12602, i32* %t12600, align 4
  %t12595 = getelementptr inbounds i8, i8* %stackTop.125, i64 100
  %t12596 = bitcast i8* %t12595 to i32*
  %t12597 = load i32, i32* %t12596, align 4
  %cond8 = icmp eq i32 %t12602, 0
  br i1 %cond8, label %L_1721, label %L_974

L_994:                                            ; preds = %L_991, %L_1720
  %stackTop.125 = phi i8* [ %t12506, %L_1720 ], [ %stackTop.1273472, %L_991 ]
  %frontier.124 = phi i8* [ %t12543, %L_1720 ], [ %frontier.1263473, %L_991 ]
  %t12599 = getelementptr inbounds i8, i8* %stackTop.125, i64 96
  %t12600 = bitcast i8* %t12599 to i32*
  %t12601 = load i32, i32* %t12600, align 4
  %t12608.not = icmp eq i32 %t12601, -2147483648
  br i1 %t12608.not, label %L_996, label %L_995

L_991:                                            ; preds = %loop_42
  store i32 %storemerge, i32* %t12745, align 4
  %t12613 = load i8*, i8** %t14203, align 8
  %t12615.not = icmp ult i8* %t12613, %frontier.1263473
  br i1 %t12615.not, label %L_1720, label %L_994

L_981:                                            ; preds = %L_980, %L_996, %L_1020, %L_1026, %L_1029, %L_1032, %L_1034, %L_1039, %L_1042, %L_1045, %L_1062, %L_1071, %L_1073, %L_1084, %L_1117, %L_1122, %L_1143, %L_1145, %L_1158, %L_1164, %L_1227, %L_1239, %L_1266, %L_1272, %L_1278, %L_1280, %L_1285, %L_1303, %L_1304, %L_1305, %L_1307, %L_1308, %L_1309, %L_1312, %L_1314, %L_1326, %L_1343, %L_1344, %L_1348, %L_1351, %L_1369, %L_1372, %L_1427, %L_1428, %L_1431, %L_1434, %L_1435, %L_1438, %L_1439, %L_1484, %L_1487, %L_1488, %L_1490, %L_1498, %L_1499, %L_1503, %L_1524, %L_1525, %L_1529, %L_1538, %L_1539, %L_1541, %L_1545, %L_1556, %L_1557, %L_1561, %L_1562, %L_1565, %L_1566
  %TP_1.21 = phi i8* [ inttoptr (i64 2 to i8*), %L_1566 ], [ getelementptr (i8, i8* @staticHeapI, i64 9008), %L_1565 ], [ inttoptr (i64 2 to i8*), %L_1562 ], [ getelementptr (i8, i8* @staticHeapI, i64 9008), %L_1561 ], [ inttoptr (i64 2 to i8*), %L_1557 ], [ getelementptr (i8, i8* @staticHeapI, i64 9008), %L_1556 ], [ inttoptr (i64 6 to i8*), %L_1545 ], [ inttoptr (i64 6 to i8*), %L_1541 ], [ %t9267, %L_1266 ], [ inttoptr (i64 1 to i8*), %L_1084 ], [ inttoptr (i64 6 to i8*), %L_1073 ], [ inttoptr (i64 6 to i8*), %L_1071 ], [ inttoptr (i64 6 to i8*), %L_1062 ], [ inttoptr (i64 1 to i8*), %L_1045 ], [ inttoptr (i64 6 to i8*), %L_1042 ], [ inttoptr (i64 6 to i8*), %L_1039 ], [ inttoptr (i64 6 to i8*), %L_1034 ], [ inttoptr (i64 6 to i8*), %L_1032 ], [ inttoptr (i64 6 to i8*), %L_1029 ], [ inttoptr (i64 6 to i8*), %L_1026 ], [ inttoptr (i64 6 to i8*), %L_1020 ], [ inttoptr (i64 1 to i8*), %L_996 ], [ inttoptr (i64 1 to i8*), %L_980 ], [ inttoptr (i64 2 to i8*), %L_1539 ], [ inttoptr (i64 2 to i8*), %L_1538 ], [ getelementptr (i8, i8* @staticHeapI, i64 9200), %L_1529 ], [ inttoptr (i64 2 to i8*), %L_1525 ], [ getelementptr (i8, i8* @staticHeapI, i64 9008), %L_1524 ], [ inttoptr (i64 6 to i8*), %L_1145 ], [ getelementptr (i8, i8* @staticHeapI, i64 9184), %L_1143 ], [ inttoptr (i64 6 to i8*), %L_1503 ], [ inttoptr (i64 2 to i8*), %L_1499 ], [ getelementptr (i8, i8* @staticHeapI, i64 9008), %L_1498 ], [ getelementptr (i8, i8* @staticHeapI, i64 9040), %L_1122 ], [ inttoptr (i64 3 to i8*), %L_1117 ], [ inttoptr (i64 1 to i8*), %L_1490 ], [ inttoptr (i64 2 to i8*), %L_1488 ], [ getelementptr (i8, i8* @staticHeapI, i64 9008), %L_1487 ], [ inttoptr (i64 2 to i8*), %L_1484 ], [ inttoptr (i64 1 to i8*), %L_1164 ], [ inttoptr (i64 1 to i8*), %L_1158 ], [ inttoptr (i64 6 to i8*), %L_1439 ], [ inttoptr (i64 6 to i8*), %L_1438 ], [ inttoptr (i64 6 to i8*), %L_1435 ], [ inttoptr (i64 6 to i8*), %L_1434 ], [ inttoptr (i64 6 to i8*), %L_1431 ], [ inttoptr (i64 6 to i8*), %L_1428 ], [ inttoptr (i64 6 to i8*), %L_1427 ], [ inttoptr (i64 1 to i8*), %L_1372 ], [ inttoptr (i64 1 to i8*), %L_1369 ], [ inttoptr (i64 1 to i8*), %L_1351 ], [ inttoptr (i64 1 to i8*), %L_1348 ], [ inttoptr (i64 7 to i8*), %L_1344 ], [ inttoptr (i64 1 to i8*), %L_1343 ], [ inttoptr (i64 7 to i8*), %L_1326 ], [ getelementptr (i8, i8* @staticHeapI, i64 8968), %L_1314 ], [ inttoptr (i64 2 to i8*), %L_1312 ], [ getelementptr (i8, i8* @staticHeapI, i64 8968), %L_1309 ], [ inttoptr (i64 1 to i8*), %L_1308 ], [ inttoptr (i64 2 to i8*), %L_1307 ], [ inttoptr (i64 1 to i8*), %L_1305 ], [ inttoptr (i64 1 to i8*), %L_1304 ], [ inttoptr (i64 2 to i8*), %L_1303 ], [ inttoptr (i64 1 to i8*), %L_1280 ], [ inttoptr (i64 6 to i8*), %L_1278 ], [ inttoptr (i64 6 to i8*), %L_1285 ], [ getelementptr (i8, i8* @staticHeapI, i64 9040), %L_1239 ], [ getelementptr (i8, i8* @staticHeapI, i64 9056), %L_1227 ], [ %t9209, %L_1272 ]
  %TP_0.64.in.in = phi i8* [ %t5245, %L_1566 ], [ %t5257, %L_1565 ], [ %t5281, %L_1562 ], [ %t5293, %L_1561 ], [ %t5317, %L_1557 ], [ %t5329, %L_1556 ], [ %t5432, %L_1545 ], [ %t5464, %L_1541 ], [ %t9277, %L_1266 ], [ %t11670, %L_1084 ], [ %t11753, %L_1073 ], [ %t11778, %L_1071 ], [ %t11867, %L_1062 ], [ %t12031, %L_1045 ], [ %t12052, %L_1042 ], [ %t12078, %L_1039 ], [ %t12114, %L_1034 ], [ %t12139, %L_1032 ], [ %t12165, %L_1029 ], [ %t12198, %L_1026 ], [ %t12254, %L_1020 ], [ %t12555, %L_996 ], [ %t12648, %L_980 ], [ %t5484, %L_1539 ], [ %t5496, %L_1538 ], [ %t5765, %L_1529 ], [ %t5959, %L_1525 ], [ %t5971, %L_1524 ], [ %t10834, %L_1145 ], [ %t10859, %L_1143 ], [ %t6245, %L_1503 ], [ %t6362, %L_1499 ], [ %t6374, %L_1498 ], [ %t11124, %L_1122 ], [ %t11185, %L_1117 ], [ %t6497, %L_1490 ], [ %t6509, %L_1488 ], [ %t6521, %L_1487 ], [ %t6576, %L_1484 ], [ %t10557, %L_1164 ], [ %t10631, %L_1158 ], [ %t7469, %L_1439 ], [ %t7481, %L_1438 ], [ %t7508, %L_1435 ], [ %t7520, %L_1434 ], [ %t7542, %L_1431 ], [ %t7569, %L_1428 ], [ %t7581, %L_1427 ], [ %t8122, %L_1372 ], [ %t8149, %L_1369 ], [ %t8272, %L_1351 ], [ %t8299, %L_1348 ], [ %t8327, %L_1344 ], [ %t8339, %L_1343 ], [ %t8477, %L_1326 ], [ %t8533, %L_1314 ], [ %t8554, %L_1312 ], [ %t8602, %L_1309 ], [ %t8616, %L_1308 ], [ %t8628, %L_1307 ], [ %t8651, %L_1305 ], [ %t8663, %L_1304 ], [ %t8675, %L_1303 ], [ %t9095, %L_1280 ], [ %t9118, %L_1278 ], [ %t9053, %L_1285 ], [ %t9540, %L_1239 ], [ %t9700, %L_1227 ], [ %t9241, %L_1272 ]
  %stackTop.126 = phi i8* [ %stackTop.128, %L_1566 ], [ %stackTop.128, %L_1565 ], [ %stackTop.128, %L_1562 ], [ %stackTop.128, %L_1561 ], [ %stackTop.125, %L_1557 ], [ %t12406.le, %L_1556 ], [ %t11602.le, %L_1545 ], [ %t11487.le, %L_1541 ], [ %t9263, %L_1266 ], [ %t12339.le3658, %L_1084 ], [ %t12339.le3646, %L_1073 ], [ %t12339.le3648, %L_1071 ], [ %t12339.le3654, %L_1062 ], [ %t12339.le3678, %L_1045 ], [ %t12339.le3670, %L_1042 ], [ %t12339.le3672, %L_1039 ], [ %t12339.le3674, %L_1034 ], [ %t12339.le3676, %L_1032 ], [ %t12339.le3662, %L_1029 ], [ %t12339.le3664, %L_1026 ], [ %t12339.le3668, %L_1020 ], [ %stackTop.125, %L_996 ], [ %stackTop.1273472, %L_980 ], [ %t11354, %L_1539 ], [ %stackTop.1243509, %L_1538 ], [ %stackTop.122, %L_1529 ], [ %stackTop.62, %L_1525 ], [ %stackTop.612601, %L_1524 ], [ %stackTop.115, %L_1145 ], [ %stackTop.114, %L_1143 ], [ %stackTop.120, %L_1503 ], [ %stackTop.118, %L_1499 ], [ %stackTop.1172622, %L_1498 ], [ %stackTop.120, %L_1122 ], [ %stackTop.122, %L_1117 ], [ %t10771, %L_1490 ], [ %stackTop.111, %L_1488 ], [ %t10434, %L_1487 ], [ %stackTop.110, %L_1484 ], [ %stackTop.111, %L_1164 ], [ %stackTop.1123590, %L_1158 ], [ %stackTop.843552, %L_1439 ], [ %stackTop.843552, %L_1438 ], [ %stackTop.843552, %L_1435 ], [ %stackTop.843552, %L_1434 ], [ %stackTop.843552, %L_1431 ], [ %stackTop.843552, %L_1428 ], [ %stackTop.843552, %L_1427 ], [ %t9958, %L_1372 ], [ %t9958, %L_1369 ], [ %stackTop.100, %L_1351 ], [ %stackTop.100, %L_1348 ], [ %stackTop.100, %L_1344 ], [ %stackTop.100, %L_1343 ], [ %stackTop.100, %L_1326 ], [ %t9741, %L_1314 ], [ %t9741, %L_1312 ], [ %stackTop.98, %L_1309 ], [ %stackTop.98, %L_1308 ], [ %stackTop.98, %L_1307 ], [ %t8959, %L_1305 ], [ %t8959, %L_1304 ], [ %t8959, %L_1303 ], [ %t9424, %L_1280 ], [ %t9424, %L_1278 ], [ %t9472, %L_1285 ], [ %stackTop.98, %L_1239 ], [ %t9741, %L_1227 ], [ %stackTop.92, %L_1272 ]
  %frontier.125 = phi i8* [ %t12939, %L_1566 ], [ %t12939, %L_1565 ], [ %t12939, %L_1562 ], [ %t12939, %L_1561 ], [ %t12567, %L_1557 ], [ %t12442, %L_1556 ], [ %t11634, %L_1545 ], [ %t11519, %L_1541 ], [ %frontier.94, %L_1266 ], [ %t12371, %L_1084 ], [ %t12371, %L_1073 ], [ %t12371, %L_1071 ], [ %t12371, %L_1062 ], [ %t12371, %L_1045 ], [ %t12371, %L_1042 ], [ %t12371, %L_1039 ], [ %t12371, %L_1034 ], [ %t12371, %L_1032 ], [ %t12371, %L_1029 ], [ %t12371, %L_1026 ], [ %t12371, %L_1020 ], [ %frontier.124, %L_996 ], [ %frontier.1263473, %L_980 ], [ %frontier.0, %L_1539 ], [ %frontier.1233510, %L_1538 ], [ %frontier.121, %L_1529 ], [ %t6116, %L_1525 ], [ %frontier.632603, %L_1524 ], [ %t10873, %L_1145 ], [ %frontier.113, %L_1143 ], [ %frontier.119, %L_1503 ], [ %t11028, %L_1499 ], [ %frontier.1162624, %L_1498 ], [ %frontier.119, %L_1122 ], [ %frontier.121, %L_1117 ], [ %frontier.0, %L_1490 ], [ %t10569, %L_1488 ], [ %t10467, %L_1487 ], [ %t10399, %L_1484 ], [ %frontier.111, %L_1164 ], [ %frontier.1123591, %L_1158 ], [ %frontier.853554, %L_1439 ], [ %frontier.853554, %L_1438 ], [ %frontier.853554, %L_1435 ], [ %frontier.853554, %L_1434 ], [ %frontier.853554, %L_1431 ], [ %frontier.853554, %L_1428 ], [ %frontier.853554, %L_1427 ], [ %frontier.0, %L_1372 ], [ %frontier.0, %L_1369 ], [ %frontier.100, %L_1351 ], [ %frontier.100, %L_1348 ], [ %frontier.100, %L_1344 ], [ %frontier.100, %L_1343 ], [ %frontier.100, %L_1326 ], [ %frontier.0, %L_1314 ], [ %frontier.0, %L_1312 ], [ %frontier.98, %L_1309 ], [ %frontier.98, %L_1308 ], [ %frontier.98, %L_1307 ], [ %t8979, %L_1305 ], [ %t8979, %L_1304 ], [ %t8979, %L_1303 ], [ %t9445, %L_1280 ], [ %t9445, %L_1278 ], [ %t9492, %L_1285 ], [ %frontier.98, %L_1239 ], [ %frontier.0, %L_1227 ], [ %t9216, %L_1272 ]
  %TP_0.64.in = bitcast i8* %TP_0.64.in.in to i8**
  %TP_0.64 = load i8*, i8** %TP_0.64.in, align 8
  %t12619 = getelementptr inbounds i8, i8* %stackTop.126, i64 8
  %t12620 = bitcast i8* %t12619 to i64*
  store i64 9, i64* %t12620, align 4
  %t12622 = getelementptr inbounds i8, i8* %stackTop.126, i64 40
  %t12623 = bitcast i8* %t12622 to i8**
  store i8* %TP_1.21, i8** %t12623, align 8
  %t12626 = getelementptr inbounds i8, i8* %stackTop.126, i64 48
  %t12627 = bitcast i8* %t12626 to i8**
  store i8* %TP_0.64, i8** %t12627, align 8
  %t12630 = getelementptr inbounds i8, i8* %stackTop.126, i64 32
  %t12631 = bitcast i8* %t12630 to i64*
  store i64 38, i64* %t12631, align 4
  store i8* %frontier.125, i8** %t15197, align 8
  store i8* %t12622, i8** %t15200, align 8
  br label %common.ret

L_980:                                            ; preds = %loop_42
  %t12745.le = bitcast i8* %t12744 to i32*
  store i32 %storemerge, i32* %t12745.le, align 4
  %t12641 = getelementptr inbounds i8, i8* %stackTop.1273472, i64 24
  %t12642 = bitcast i8* %t12641 to i8**
  %t12644 = getelementptr inbounds i8, i8* %stackTop.1273472, i64 160
  %t12645 = bitcast i8* %t12644 to i8**
  %t12646 = load i8*, i8** %t12645, align 8
  store i8* %t12646, i8** %t12642, align 8
  %t12648 = getelementptr inbounds i8, i8* %stackTop.1273472, i64 56
  br label %L_981

L_976:                                            ; preds = %loop_42
  %t12728 = add nsw i32 %TW32_0.44, -1
  %t12711.frozen = freeze i32 %t12711
  %t12719 = udiv i32 %t12711.frozen, 44488
  %130 = mul i32 %t12719, 44488
  %t12712.decomposed = sub i32 %t12711.frozen, %130
  %t12714 = mul nuw nsw i32 %t12712.decomposed, 48271
  %t12721 = mul nuw nsw i32 %t12719, 3399
  %t12724.not = icmp ugt i32 %t12714, %t12721
  %t12703 = xor i32 %t12721, 2147483647
  %131 = sub nsw i32 0, %t12721
  %TW32_0.43.p = select i1 %t12724.not, i32 %131, i32 %t12703
  %TW32_0.43 = add i32 %TW32_0.43.p, %t12714
  %t12660 = shl i32 %t12658, 18
  %t12666 = xor i32 %t12660, %t12658
  %t12669 = lshr i32 %t12666, 13
  %t12672 = xor i32 %t12669, %t12666
  %t12678 = lshr i32 %storemerge, 1
  %t12680 = and i32 %t12678, 1073741823
  %t12683 = xor i32 %TW32_0.43, %t12666
  %t12685 = and i32 %t12683, 1073741824
  %t12688 = or i32 %t12685, %t12680
  store i32 %t12672, i32* %t12737, align 4
  store i32 %TW32_0.43, i32* %t12741, align 4
  br label %loop_42

loop_42:                                          ; preds = %L_974, %L_976
  %t12658 = phi i32 [ %TW32_0.453470, %L_974 ], [ %t12672, %L_976 ]
  %t12711 = phi i32 [ %TW32_1.103471, %L_974 ], [ %TW32_0.43, %L_976 ]
  %storemerge = phi i32 [ 0, %L_974 ], [ %t12688, %L_976 ]
  %TW32_0.44 = phi i32 [ 31, %L_974 ], [ %t12728, %L_976 ]
  switch i32 %TW32_0.44, label %L_976 [
    i32 0, label %L_991
    i32 -2147483648, label %L_980
  ]

L_974:                                            ; preds = %L_974.preheader, %L_995
  %frontier.1263473 = phi i8* [ %t12567, %L_995 ], [ %t12939, %L_974.preheader ]
  %stackTop.1273472 = phi i8* [ %stackTop.125, %L_995 ], [ %stackTop.128, %L_974.preheader ]
  %TW32_1.103471 = phi i32 [ %t12585, %L_995 ], [ 247, %L_974.preheader ]
  %TW32_0.453470 = phi i32 [ %t12597, %L_995 ], [ 73256, %L_974.preheader ]
  %t12736 = getelementptr inbounds i8, i8* %stackTop.1273472, i64 100
  %t12737 = bitcast i8* %t12736 to i32*
  store i32 %TW32_0.453470, i32* %t12737, align 4
  %t12740 = getelementptr inbounds i8, i8* %stackTop.1273472, i64 136
  %t12741 = bitcast i8* %t12740 to i32*
  store i32 %TW32_1.103471, i32* %t12741, align 4
  %t12744 = getelementptr inbounds i8, i8* %stackTop.1273472, i64 140
  %t12745 = bitcast i8* %t12744 to i32*
  br label %loop_42

L_974.preheader:                                  ; preds = %L_1560, %loop_40.preheader
  store i8* %t12970, i8** %t12865, align 8
  store i8* %t12974, i8** %t12969, align 8
  store i8* inttoptr (i64 1 to i8*), i8** %t12948, align 8
  %t12766 = bitcast i8* %t12957 to i32*
  store i32 48, i32* %t12766, align 4
  br label %L_974

L_1716:                                           ; preds = %L_970
  %trunc1858 = icmp sgt i32 %t12791, -1
  br i1 %trunc1858, label %loop_40.preheader, label %L_1562

loop_40.preheader:                                ; preds = %L_1716
  %t12779 = sext i32 %t12791 to i64
  %t12769.not3462.not = icmp eq i32 %t12791, 0
  br i1 %t12769.not3462.not, label %L_974.preheader, label %L_1559.preheader

L_1559.preheader:                                 ; preds = %L_969.preheader, %loop_40.preheader
  %t1277946264630 = phi i64 [ %t12779, %loop_40.preheader ], [ 1, %L_969.preheader ]
  br label %L_1559

L_970:                                            ; preds = %L_969.preheader, %L_970
  %t127913460 = phi i32 [ %t12791, %L_970 ], [ 1, %L_969.preheader ]
  %TP_0.663459 = phi i8* [ %t12787, %L_970 ], [ getelementptr (i8, i8* @staticHeapI, i64 8904), %L_969.preheader ]
  %t12786 = bitcast i8* %TP_0.663459 to i8**
  %t12787 = load i8*, i8** %t12786, align 8
  %t12791 = add i32 %t127913460, 1
  %cond6 = icmp eq i8* %t12787, inttoptr (i64 1 to i8*)
  br i1 %cond6, label %L_1716, label %L_970

L_969.preheader:                                  ; preds = %L_1564, %loop_39.preheader
  br i1 icmp eq (i8* getelementptr (i8, i8* @staticHeapI, i64 8904), i8* inttoptr (i64 1 to i8*)), label %L_1559.preheader, label %L_970

L_1714:                                           ; preds = %L_965
  %trunc1856 = icmp sgt i32 %t12824, -1
  br i1 %trunc1856, label %loop_39.preheader, label %L_1566

loop_39.preheader:                                ; preds = %L_1714
  %t12812 = sext i32 %t12824 to i64
  %t12802.not3456.not = icmp eq i32 %t12824, 0
  br i1 %t12802.not3456.not, label %L_969.preheader, label %L_1563.preheader

L_1563.preheader:                                 ; preds = %L_963, %loop_39.preheader
  %t1281246354639 = phi i64 [ %t12812, %loop_39.preheader ], [ 1, %L_963 ]
  br label %L_1563

L_965:                                            ; preds = %L_963, %L_965
  %t128243455 = phi i32 [ %t12824, %L_965 ], [ 1, %L_963 ]
  %TP_0.683454 = phi i8* [ %t12820, %L_965 ], [ getelementptr (i8, i8* @staticHeapI, i64 8904), %L_963 ]
  %t12819 = bitcast i8* %TP_0.683454 to i8**
  %t12820 = load i8*, i8** %t12819, align 8
  %t12824 = add i32 %t128243455, 1
  %cond4 = icmp eq i8* %t12820, inttoptr (i64 1 to i8*)
  br i1 %cond4, label %L_1714, label %L_965

L_963:                                            ; preds = %L_956, %L_961
  %stackTop.128 = phi i8* [ %t5218, %L_961 ], [ %t13001, %L_956 ]
  %frontier.127 = phi i8* [ %t5233, %L_961 ], [ %t13026, %L_956 ]
  %t12828 = getelementptr inbounds i8, i8* %frontier.127, i64 8
  %t12833 = bitcast i8* %frontier.127 to i64*
  store i64 37, i64* %t12833, align 4
  store i8 0, i8* %t12828, align 1
  %t12841 = getelementptr inbounds i8, i8* %frontier.127, i64 24
  %t12845 = getelementptr inbounds i8, i8* %frontier.127, i64 16
  %t12846 = bitcast i8* %t12845 to i64*
  store i64 39, i64* %t12846, align 4
  %t12851 = bitcast i8* %t12841 to i8**
  store i8* %t12828, i8** %t12851, align 8
  %t12854 = getelementptr inbounds i8, i8* %frontier.127, i64 32
  %t12855 = bitcast i8* %t12854 to i8**
  %t12857 = getelementptr inbounds i8, i8* %stackTop.128, i64 144
  %t12858 = bitcast i8* %t12857 to i8**
  %t12859 = load i8*, i8** %t12858, align 8
  store i8* %t12859, i8** %t12855, align 8
  %t12861 = getelementptr inbounds i8, i8* %frontier.127, i64 40
  %t12862 = bitcast i8* %t12861 to i8**
  %t12864 = getelementptr inbounds i8, i8* %stackTop.128, i64 64
  %t12865 = bitcast i8* %t12864 to i8**
  %t12866 = load i8*, i8** %t12865, align 8
  store i8* %t12866, i8** %t12862, align 8
  %t12868 = getelementptr inbounds i8, i8* %stackTop.128, i64 136
  %t12869 = bitcast i8* %t12868 to i8**
  %132 = bitcast i8* %t12868 to i8***
  %t128701853 = load i8**, i8*** %132, align 8
  %t12873 = load i8*, i8** %t128701853, align 8
  %t12875 = getelementptr inbounds i8, i8* %frontier.127, i64 56
  %t12879 = getelementptr inbounds i8, i8* %frontier.127, i64 48
  %t12880 = bitcast i8* %t12879 to i64*
  store i64 83, i64* %t12880, align 4
  %t12885 = bitcast i8* %t12875 to i8**
  store i8* %t12841, i8** %t12885, align 8
  %t12888 = getelementptr inbounds i8, i8* %frontier.127, i64 64
  %t12889 = bitcast i8* %t12888 to i8**
  store i8* %t12873, i8** %t12889, align 8
  %t12894 = load i8*, i8** %t12869, align 8
  %t12895 = ptrtoint i8* %t12894 to i64
  %t12897 = lshr i64 %t12895, 8
  %t12900 = load i8*, i8** %t14171, align 8
  %t12903 = getelementptr inbounds i8, i8* %t12900, i64 %t12897
  store i8 1, i8* %t12903, align 1
  %t129091854 = load i8**, i8*** %132, align 8
  store i8* %t12875, i8** %t129091854, align 8
  %t12914 = getelementptr inbounds i8, i8* %frontier.127, i64 80
  %t12918 = getelementptr inbounds i8, i8* %frontier.127, i64 72
  %t12919 = bitcast i8* %t12918 to i64*
  store i64 35, i64* %t12919, align 4
  %t12924 = bitcast i8* %t12914 to i32*
  store i32 0, i32* %t12924, align 4
  %t12926 = getelementptr inbounds i8, i8* %frontier.127, i64 96
  %t12928 = getelementptr inbounds i8, i8* %stackTop.128, i64 160
  %t12929 = bitcast i8* %t12928 to i8**
  store i8* %t12926, i8** %t12929, align 8
  %t12936 = getelementptr inbounds i8, i8* %frontier.127, i64 88
  %t12937 = bitcast i8* %t12936 to i64*
  store i64 71, i64* %t12937, align 4
  %t12939 = getelementptr inbounds i8, i8* %frontier.127, i64 120
  %133 = bitcast i8* %t12928 to i8***
  %t129431855 = load i8**, i8*** %133, align 8
  %t12947 = getelementptr inbounds i8, i8* %stackTop.128, i64 88
  %t12948 = bitcast i8* %t12947 to i8**
  %t12949 = load i8*, i8** %t12948, align 8
  store i8* %t12949, i8** %t129431855, align 8
  %t12953 = load i8*, i8** %t12929, align 8
  %t12954 = getelementptr inbounds i8, i8* %t12953, i64 8
  %t12955 = bitcast i8* %t12954 to i8**
  %t12957 = getelementptr inbounds i8, i8* %stackTop.128, i64 96
  %t12958 = bitcast i8* %t12957 to i8**
  %t12959 = load i8*, i8** %t12958, align 8
  store i8* %t12959, i8** %t12955, align 8
  %t12963 = load i8*, i8** %t12929, align 8
  %t12964 = getelementptr inbounds i8, i8* %t12963, i64 16
  %t12965 = bitcast i8* %t12964 to i8**
  store i8* %t12914, i8** %t12965, align 8
  %t12968 = getelementptr inbounds i8, i8* %stackTop.128, i64 80
  %t12969 = bitcast i8* %t12968 to i8**
  %t12970 = load i8*, i8** %t12969, align 8
  %t12972 = getelementptr inbounds i8, i8* %stackTop.128, i64 152
  %t12973 = bitcast i8* %t12972 to i8**
  %t12974 = load i8*, i8** %t12973, align 8
  br i1 icmp eq (i8* getelementptr (i8, i8* @staticHeapI, i64 8904), i8* inttoptr (i64 1 to i8*)), label %L_1563.preheader, label %L_965

L_956:                                            ; preds = %L_1712
  %t13033 = add i32 %t13046, -1
  store i32 %t13033, i32* %t13969, align 4
  %t13038 = getelementptr inbounds i8, i8* %stackTop.0, i64 -16
  %t13039 = bitcast i8* %t13038 to i32*
  %t13040 = load i32, i32* %t13039, align 4
  %t13041 = tail call i32 @Posix_FileSys_ST_isReg(i32 %t13040)
  %t13014 = bitcast i8* %t13038 to i64*
  store i64 93, i64* %t13014, align 4
  %t13016 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %t13016, i8** %t15200, align 8
  %t13023 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 4096, i64 21)
  %t13026 = load i8*, i8** %t15197, align 8
  %t13029 = load i8*, i8** %t15200, align 8
  %t13005 = getelementptr inbounds i8, i8* %t13029, i64 -96
  %t13006 = bitcast i8* %t13005 to i8**
  %t13007 = load i8*, i8** %t13006, align 8
  %t13009 = getelementptr inbounds i8, i8* %t13029, i64 -80
  %t13010 = bitcast i8* %t13009 to i8**
  %t13011 = load i8*, i8** %t13010, align 8
  call void @llvm.memset.p0i8.i64(i8* noundef nonnull align 1 dereferenceable(4096) %t13023, i8 0, i64 4096, i1 false)
  %t13001 = getelementptr inbounds i8, i8* %t13029, i64 -160
  store i8* %t13023, i8** %t13006, align 8
  store i8* %t13007, i8** %t13010, align 8
  %t12986 = getelementptr inbounds i8, i8* %t13029, i64 -8
  %t12987 = bitcast i8* %t12986 to i8**
  store i8* %t13011, i8** %t12987, align 8
  %t12991 = load i8*, i8** %t14203, align 8
  %t12993.not = icmp ult i8* %t12991, %t13026
  br i1 %t12993.not, label %L_961, label %L_963

L_1712:                                           ; preds = %doSwitchNextBlock
  %t13046 = load i32, i32* %t13969, align 4
  %t13048.not = icmp eq i32 %t13046, 0
  br i1 %t13048.not, label %L_786, label %L_956

L_1711:                                           ; preds = %doSwitchNextBlock
  %t13070 = tail call i64 @Posix_FileSys_Stat_getCTime()
  %t13054 = bitcast i8* %stackTop.0 to i64*
  store i64 %t13070, i64* %t13054, align 4
  %t13057 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  %t13058 = bitcast i8* %t13057 to i8**
  %t13060 = getelementptr inbounds i8, i8* %stackTop.0, i64 -56
  %t13061 = bitcast i8* %t13060 to i8**
  %t13062 = load i8*, i8** %t13061, align 8
  store i8* %t13062, i8** %t13058, align 8
  %t13064 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  br label %L_597.sink.split

L_1710:                                           ; preds = %doSwitchNextBlock
  %t13090 = tail call i64 @Posix_FileSys_Stat_getMTime()
  %t13074 = bitcast i8* %stackTop.0 to i64*
  store i64 %t13090, i64* %t13074, align 4
  %t13077 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  %t13078 = bitcast i8* %t13077 to i8**
  %t13080 = getelementptr inbounds i8, i8* %stackTop.0, i64 -56
  %t13081 = bitcast i8* %t13080 to i8**
  %t13082 = load i8*, i8** %t13081, align 8
  store i8* %t13082, i8** %t13078, align 8
  %t13084 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  br label %L_597.sink.split

L_946:                                            ; preds = %L_936
  %t13114 = tail call i32 @Posix_FileSys_Stat_getUId()
  %t13113 = tail call i32 @Posix_FileSys_Stat_getGId()
  %t13112 = tail call i64 @Posix_FileSys_Stat_getSize()
  %t13111 = tail call i64 @Posix_FileSys_Stat_getATime()
  %t13093 = getelementptr inbounds i8, i8* %stackTop.129, i64 8
  %t13094 = bitcast i8* %t13093 to i64*
  store i64 52, i64* %t13094, align 4
  %t13096 = getelementptr inbounds i8, i8* %stackTop.129, i64 168
  %t13097 = bitcast i8* %t13096 to i64*
  store i64 %t13111, i64* %t13097, align 4
  %t13100 = getelementptr inbounds i8, i8* %stackTop.129, i64 176
  %t13101 = bitcast i8* %t13100 to i8**
  %t13103 = getelementptr inbounds i8, i8* %stackTop.129, i64 112
  %t13104 = bitcast i8* %t13103 to i8**
  %t13105 = load i8*, i8** %t13104, align 8
  store i8* %t13105, i8** %t13101, align 8
  %t13107 = getelementptr inbounds i8, i8* %stackTop.129, i64 160
  br label %L_597.sink.split

L_942:                                            ; preds = %L_941
  %t13125 = add i32 %t13132, -1
  store i32 %t13125, i32* %t13969, align 4
  br label %L_786

L_941:                                            ; preds = %L_936, %doSwitchNextBlock
  %t13132 = load i32, i32* %t13969, align 4
  %t13134.not = icmp eq i32 %t13132, 0
  br i1 %t13134.not, label %L_786, label %L_942

L_936:                                            ; preds = %isReg_0
  %t13154 = tail call i64 @Posix_FileSys_Stat_getDev()
  %t13153 = tail call i64 @Posix_FileSys_Stat_getINo()
  %t13152 = tail call i32 @Posix_FileSys_Stat_getMode()
  %t13149 = getelementptr inbounds i8, i8* %stackTop.129, i64 152
  %t13150 = bitcast i8* %t13149 to i32*
  store i32 %t13152, i32* %t13150, align 4
  %t13151 = tail call i64 @Posix_FileSys_Stat_getNLink()
  %134 = icmp ult i64 %t13151, 2147483648
  br i1 %134, label %L_946, label %L_941

isReg_0:                                          ; preds = %L_929, %L_1709
  %stackTop.129 = phi i8* [ %t13209, %L_929 ], [ %t13169, %L_1709 ]
  %t13162 = load i32, i32* %t13969, align 4
  %t13163 = add i32 %t13162, 1
  store i32 %t13163, i32* %t13969, align 4
  %t13167 = tail call i32 @Posix_FileSys_Stat_fstat(i32 0)
  %t13157.not = icmp eq i32 %t13167, -1
  br i1 %t13157.not, label %L_1570, label %L_936

L_1709:                                           ; preds = %doSwitchNextBlock
  %t13169 = getelementptr inbounds i8, i8* %stackTop.0, i64 -160
  br label %isReg_0

L_932:                                            ; preds = %L_929
  %t13171 = getelementptr inbounds i8, i8* %stackTop.0, i64 -160
  %t13172 = bitcast i8* %t13171 to i64*
  store i64 51, i64* %t13172, align 4
  %t13174 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t13175 = bitcast i8* %t13174 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t13175, align 8
  %t13179 = bitcast i8* %stackTop.0 to i32*
  store i32 0, i32* %t13179, align 4
  %t13182 = bitcast i8* %t13204 to i64*
  store i64 71, i64* %t13182, align 4
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %t13174, i8** %t15200, align 8
  br label %common.ret

L_929:                                            ; preds = %L_1708
  %t13209 = getelementptr inbounds i8, i8* %stackTop.0, i64 -168
  %t13199 = add i32 %t13212, -1
  store i32 %t13199, i32* %t13969, align 4
  %t13204 = getelementptr inbounds i8, i8* %stackTop.0, i64 -16
  %t13205 = bitcast i8* %t13204 to i32*
  %t13206 = load i32, i32* %t13205, align 4
  %t13207 = tail call i32 @Posix_FileSys_ST_isReg(i32 %t13206)
  %t13193.not = icmp eq i32 %t13207, 0
  br i1 %t13193.not, label %isReg_0, label %L_932

L_1708:                                           ; preds = %doSwitchNextBlock
  %t13212 = load i32, i32* %t13969, align 4
  %t13214.not = icmp eq i32 %t13212, 0
  br i1 %t13214.not, label %L_786, label %L_929

L_1707:                                           ; preds = %doSwitchNextBlock
  %t13236 = tail call i64 @Posix_FileSys_Stat_getCTime()
  %t13220 = bitcast i8* %stackTop.0 to i64*
  store i64 %t13236, i64* %t13220, align 4
  %t13223 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  %t13224 = bitcast i8* %t13223 to i8**
  %t13226 = getelementptr inbounds i8, i8* %stackTop.0, i64 -56
  %t13227 = bitcast i8* %t13226 to i8**
  %t13228 = load i8*, i8** %t13227, align 8
  store i8* %t13228, i8** %t13224, align 8
  %t13230 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  br label %L_597.sink.split

L_1706:                                           ; preds = %doSwitchNextBlock
  %t13256 = tail call i64 @Posix_FileSys_Stat_getMTime()
  %t13240 = bitcast i8* %stackTop.0 to i64*
  store i64 %t13256, i64* %t13240, align 4
  %t13243 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  %t13244 = bitcast i8* %t13243 to i8**
  %t13246 = getelementptr inbounds i8, i8* %stackTop.0, i64 -56
  %t13247 = bitcast i8* %t13246 to i8**
  %t13248 = load i8*, i8** %t13247, align 8
  store i8* %t13248, i8** %t13244, align 8
  %t13250 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  br label %L_597.sink.split

L_919:                                            ; preds = %L_909
  %t13280 = tail call i32 @Posix_FileSys_Stat_getUId()
  %t13279 = tail call i32 @Posix_FileSys_Stat_getGId()
  %t13278 = tail call i64 @Posix_FileSys_Stat_getSize()
  %t13277 = tail call i64 @Posix_FileSys_Stat_getATime()
  %t13259 = getelementptr inbounds i8, i8* %stackTop.130, i64 8
  %t13260 = bitcast i8* %t13259 to i64*
  store i64 50, i64* %t13260, align 4
  %t13262 = getelementptr inbounds i8, i8* %stackTop.130, i64 168
  %t13263 = bitcast i8* %t13262 to i64*
  store i64 %t13277, i64* %t13263, align 4
  %t13266 = getelementptr inbounds i8, i8* %stackTop.130, i64 176
  %t13267 = bitcast i8* %t13266 to i8**
  %t13269 = getelementptr inbounds i8, i8* %stackTop.130, i64 112
  %t13270 = bitcast i8* %t13269 to i8**
  %t13271 = load i8*, i8** %t13270, align 8
  store i8* %t13271, i8** %t13267, align 8
  %t13273 = getelementptr inbounds i8, i8* %stackTop.130, i64 160
  br label %L_597.sink.split

L_915:                                            ; preds = %L_914
  %t13291 = add i32 %t13298, -1
  store i32 %t13291, i32* %t13969, align 4
  br label %L_786

L_914:                                            ; preds = %L_909, %doSwitchNextBlock
  %t13298 = load i32, i32* %t13969, align 4
  %t13300.not = icmp eq i32 %t13298, 0
  br i1 %t13300.not, label %L_786, label %L_915

L_909:                                            ; preds = %L_906
  %t13320 = tail call i64 @Posix_FileSys_Stat_getDev()
  %t13319 = tail call i64 @Posix_FileSys_Stat_getINo()
  %t13318 = tail call i32 @Posix_FileSys_Stat_getMode()
  %t13315 = getelementptr inbounds i8, i8* %stackTop.130, i64 152
  %t13316 = bitcast i8* %t13315 to i32*
  store i32 %t13318, i32* %t13316, align 4
  %t13317 = tail call i64 @Posix_FileSys_Stat_getNLink()
  %135 = icmp ult i64 %t13317, 2147483648
  br i1 %135, label %L_919, label %L_914

L_906:                                            ; preds = %L_1704, %L_904
  %stackTop.130 = phi i8* [ %t4721, %L_904 ], [ %stackTop.131, %L_1704 ]
  %frontier.128 = phi i8* [ %t4736, %L_904 ], [ %t13575, %L_1704 ]
  %t13327 = getelementptr inbounds i8, i8* %frontier.128, i64 8
  %t13329 = getelementptr inbounds i8, i8* %stackTop.130, i64 136
  %t13330 = bitcast i8* %t13329 to i8**
  store i8* %t13327, i8** %t13330, align 8
  %t13338 = bitcast i8* %frontier.128 to i64*
  store i64 25, i64* %t13338, align 4
  %136 = bitcast i8* %t13329 to i8***
  %t133441817 = load i8**, i8*** %136, align 8
  store i8* inttoptr (i64 1 to i8*), i8** %t133441817, align 8
  %t13349 = getelementptr inbounds i8, i8* %stackTop.130, i64 96
  %t13350 = bitcast i8* %t13349 to i8**
  %137 = bitcast i8* %t13349 to i8***
  %t133511818 = load i8**, i8*** %137, align 8
  %t13354 = load i8*, i8** %t133511818, align 8
  %t13356 = getelementptr inbounds i8, i8* %frontier.128, i64 24
  %t13360 = getelementptr inbounds i8, i8* %frontier.128, i64 16
  %t13361 = bitcast i8* %t13360 to i64*
  store i64 105, i64* %t13361, align 4
  %t13366 = bitcast i8* %t13356 to i8**
  %t13370 = load i8*, i8** %t13330, align 8
  store i8* %t13370, i8** %t13366, align 8
  %t13372 = getelementptr inbounds i8, i8* %frontier.128, i64 40
  %t13376 = getelementptr inbounds i8, i8* %frontier.128, i64 32
  %t13377 = bitcast i8* %t13376 to i64*
  store i64 133, i64* %t13377, align 4
  %t13382 = bitcast i8* %t13372 to i8**
  store i8* %t13356, i8** %t13382, align 8
  %t13386 = getelementptr inbounds i8, i8* %frontier.128, i64 48
  %t13387 = bitcast i8* %t13386 to i8**
  store i8* %t13354, i8** %t13387, align 8
  %t13392 = load i8*, i8** %t13350, align 8
  %t13393 = ptrtoint i8* %t13392 to i64
  %t13395 = lshr i64 %t13393, 8
  %t13398 = load i8*, i8** %t14171, align 8
  %t13401 = getelementptr inbounds i8, i8* %t13398, i64 %t13395
  store i8 1, i8* %t13401, align 1
  %t134071819 = load i8**, i8*** %137, align 8
  store i8* %t13372, i8** %t134071819, align 8
  %t13412 = getelementptr inbounds i8, i8* %frontier.128, i64 64
  %t13414 = getelementptr inbounds i8, i8* %stackTop.130, i64 144
  %t13415 = bitcast i8* %t13414 to i8**
  store i8* %t13412, i8** %t13415, align 8
  %t13422 = getelementptr inbounds i8, i8* %frontier.128, i64 56
  %t13423 = bitcast i8* %t13422 to i64*
  store i64 35, i64* %t13423, align 4
  %t13425 = getelementptr inbounds i8, i8* %frontier.128, i64 72
  %138 = bitcast i8* %t13414 to i32**
  %t134291820 = load i32*, i32** %138, align 8
  store i32 0, i32* %t134291820, align 4
  %t13434 = load i32, i32* %t13969, align 4
  %t13435 = add i32 %t13434, 1
  store i32 %t13435, i32* %t13969, align 4
  %t13439 = tail call i32 @Posix_FileSys_Stat_fstat(i32 0)
  %t13323.not = icmp eq i32 %t13439, -1
  br i1 %t13323.not, label %L_1595, label %L_909

L_1704:                                           ; preds = %L_901, %L_899
  %t13450 = load i8*, i8** %t14203, align 8
  %t13452.not = icmp ult i8* %t13450, %t13575
  br i1 %t13452.not, label %L_904, label %L_906

L_901:                                            ; preds = %L_899, %L_901
  %TP_0.693452 = phi i8* [ %t13466, %L_901 ], [ getelementptr (i8, i8* @staticHeapI, i64 8752), %L_899 ]
  %t13465 = bitcast i8* %TP_0.693452 to i8**
  %t13466 = load i8*, i8** %t13465, align 8
  %cond1 = icmp eq i8* %t13466, inttoptr (i64 1 to i8*)
  br i1 %cond1, label %L_1704, label %L_901

L_899:                                            ; preds = %L_898, %L_1624
  %t13565.pre-phi = phi i8** [ %t13614, %L_898 ], [ %t4632, %L_1624 ]
  %TP_0.70 = phi i8* [ %t13594, %L_898 ], [ %t4619, %L_1624 ]
  %t4617.pn.pn = phi i8* [ %t4716.pn, %L_898 ], [ %t4617.pn, %L_1624 ]
  %frontier.53.pn = phi i8* [ %frontier.130, %L_898 ], [ %frontier.53, %L_1624 ]
  %stackTop.131 = getelementptr inbounds i8, i8* %t4617.pn.pn, i64 -184
  %t13471 = getelementptr inbounds i8, i8* %frontier.53.pn, i64 32
  %t13475 = getelementptr inbounds i8, i8* %frontier.53.pn, i64 24
  %t13476 = bitcast i8* %t13475 to i64*
  store i64 47, i64* %t13476, align 4
  %t13481 = bitcast i8* %t13471 to i8**
  store i8* %TP_0.70, i8** %t13481, align 8
  %t13484 = getelementptr inbounds i8, i8* %frontier.53.pn, i64 48
  %t13488 = getelementptr inbounds i8, i8* %frontier.53.pn, i64 40
  %t13489 = bitcast i8* %t13488 to i64*
  store i64 51, i64* %t13489, align 4
  %t13494 = bitcast i8* %t13484 to i8**
  store i8* getelementptr (i8, i8* @staticHeapI, i64 3952), i8** %t13494, align 8
  %t13498 = getelementptr inbounds i8, i8* %frontier.53.pn, i64 56
  %t13499 = bitcast i8* %t13498 to i8**
  %t13501 = getelementptr inbounds i8, i8* %t4617.pn.pn, i64 -16
  %t13502 = bitcast i8* %t13501 to i8**
  %t13503 = load i8*, i8** %t13502, align 8
  store i8* %t13503, i8** %t13499, align 8
  %t13505 = getelementptr inbounds i8, i8* %frontier.53.pn, i64 64
  %t13506 = bitcast i8* %t13505 to i8**
  store i8* %t13471, i8** %t13506, align 8
  %t13509 = getelementptr inbounds i8, i8* %frontier.53.pn, i64 72
  %t13510 = bitcast i8* %t13509 to i8**
  %t13512 = getelementptr inbounds i8, i8* %t4617.pn.pn, i64 -24
  %t13513 = bitcast i8* %t13512 to i8**
  %t13514 = load i8*, i8** %t13513, align 8
  store i8* %t13514, i8** %t13510, align 8
  %t13516 = getelementptr inbounds i8, i8* %t4617.pn.pn, i64 -48
  %t13517 = bitcast i8* %t13516 to i8**
  %139 = bitcast i8* %t13516 to i8***
  %t135181813 = load i8**, i8*** %139, align 8
  %t13521 = load i8*, i8** %t135181813, align 8
  %t13523 = getelementptr inbounds i8, i8* %frontier.53.pn, i64 88
  %t13527 = getelementptr inbounds i8, i8* %frontier.53.pn, i64 80
  %t13528 = bitcast i8* %t13527 to i64*
  store i64 81, i64* %t13528, align 4
  %t13533 = bitcast i8* %t13523 to i8**
  store i8* %t13484, i8** %t13533, align 8
  %t13536 = getelementptr inbounds i8, i8* %frontier.53.pn, i64 96
  %t13537 = bitcast i8* %t13536 to i8**
  store i8* %t13521, i8** %t13537, align 8
  %t13542 = load i8*, i8** %t13517, align 8
  %t13543 = ptrtoint i8* %t13542 to i64
  %t13545 = lshr i64 %t13543, 8
  %t13548 = load i8*, i8** %t14171, align 8
  %t13551 = getelementptr inbounds i8, i8* %t13548, i64 %t13545
  store i8 1, i8* %t13551, align 1
  %t135571814 = load i8**, i8*** %139, align 8
  store i8* %t13523, i8** %t135571814, align 8
  %t13562 = getelementptr inbounds i8, i8* %frontier.53.pn, i64 112
  %t13564 = getelementptr inbounds i8, i8* %t4617.pn.pn, i64 -152
  store i8* %t13562, i8** %t13565.pre-phi, align 8
  %t13572 = getelementptr inbounds i8, i8* %frontier.53.pn, i64 104
  %t13573 = bitcast i8* %t13572 to i64*
  store i64 69, i64* %t13573, align 4
  %t13575 = getelementptr inbounds i8, i8* %frontier.53.pn, i64 120
  %140 = bitcast i8* %t13564 to i8***
  %t135791815 = load i8**, i8*** %140, align 8
  store i8* %t13484, i8** %t135791815, align 8
  br i1 icmp eq (i8* getelementptr (i8, i8* @staticHeapI, i64 8752), i8* inttoptr (i64 1 to i8*)), label %L_1704, label %L_901

L_898:                                            ; preds = %L_893, %L_896
  %t4716.pn = phi i8* [ %t4716, %L_896 ], [ %t13664, %L_893 ]
  %frontier.130 = phi i8* [ %t4713, %L_896 ], [ %t13661, %L_893 ]
  %t13594 = getelementptr inbounds i8, i8* %frontier.130, i64 8
  %t13599 = bitcast i8* %frontier.130 to i64*
  store i64 119, i64* %t13599, align 4
  %t13604 = bitcast i8* %t13594 to i8**
  %t13606 = getelementptr inbounds i8, i8* %t4716.pn, i64 -40
  %t13607 = bitcast i8* %t13606 to i8**
  %t13608 = load i8*, i8** %t13607, align 8
  store i8* %t13608, i8** %t13604, align 8
  %t13610 = getelementptr inbounds i8, i8* %frontier.130, i64 16
  %t13611 = bitcast i8* %t13610 to i8**
  %t13613 = getelementptr inbounds i8, i8* %t4716.pn, i64 -152
  %t13614 = bitcast i8* %t13613 to i8**
  %t13615 = load i8*, i8** %t13614, align 8
  store i8* %t13615, i8** %t13611, align 8
  br label %L_899

L_893:                                            ; preds = %L_892
  store i64 92, i64* %t13649, align 4
  %t13651 = getelementptr inbounds i8, i8* %stackTop.133, i64 184
  store i8* %t13715, i8** %t15197, align 8
  store i8* %t13651, i8** %t15200, align 8
  %t13658 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 4096, i64 21)
  %t13661 = load i8*, i8** %t15197, align 8
  %t13664 = load i8*, i8** %t15200, align 8
  call void @llvm.memset.p0i8.i64(i8* noundef nonnull align 1 dereferenceable(4096) %t13658, i8 0, i64 4096, i1 false)
  %t13619 = getelementptr inbounds i8, i8* %t13664, i64 -40
  %t13620 = bitcast i8* %t13619 to i8**
  store i8* %t13658, i8** %t13620, align 8
  %t13626 = load i8*, i8** %t14203, align 8
  %t13628.not = icmp ult i8* %t13626, %t13661
  br i1 %t13628.not, label %L_896, label %L_898

L_892:                                            ; preds = %L_891, %L_1626
  %TP_0.71 = phi i8* [ %t4558, %L_1626 ], [ %t13746, %L_891 ]
  %stackTop.133 = phi i8* [ %stackTop.135, %L_1626 ], [ %stackTop.134, %L_891 ]
  %frontier.131 = phi i8* [ %t4565, %L_1626 ], [ %frontier.132, %L_891 ]
  %t13666 = getelementptr inbounds i8, i8* %frontier.131, i64 8
  %t13671 = bitcast i8* %frontier.131 to i64*
  store i64 43, i64* %t13671, align 4
  %t13676 = bitcast i8* %t13666 to i8**
  %t13678 = getelementptr inbounds i8, i8* %stackTop.133, i64 152
  %t13679 = bitcast i8* %t13678 to i8**
  %t13680 = load i8*, i8** %t13679, align 8
  store i8* %t13680, i8** %t13676, align 8
  %t13682 = getelementptr inbounds i8, i8* %frontier.131, i64 16
  %t13683 = bitcast i8* %t13682 to i8**
  store i8* %TP_0.71, i8** %t13683, align 8
  %t13686 = getelementptr inbounds i8, i8* %frontier.131, i64 32
  %t13688 = getelementptr inbounds i8, i8* %stackTop.133, i64 160
  %t13689 = bitcast i8* %t13688 to i8**
  store i8* %t13686, i8** %t13689, align 8
  %t13696 = getelementptr inbounds i8, i8* %frontier.131, i64 24
  %t13697 = bitcast i8* %t13696 to i64*
  store i64 45, i64* %t13697, align 4
  %141 = bitcast i8* %t13688 to i8***
  %t137031810 = load i8**, i8*** %141, align 8
  store i8* %t13666, i8** %t137031810, align 8
  %t13710 = load i8*, i8** %t13689, align 8
  %t13711 = getelementptr inbounds i8, i8* %t13710, i64 8
  %t13712 = bitcast i8* %t13711 to i8**
  store i8* %t13666, i8** %t13712, align 8
  %t13715 = getelementptr inbounds i8, i8* %frontier.131, i64 56
  %t13717 = getelementptr inbounds i8, i8* %stackTop.133, i64 168
  %t13718 = bitcast i8* %t13717 to i8**
  store i8* %t13715, i8** %t13718, align 8
  %t13725 = getelementptr inbounds i8, i8* %frontier.131, i64 48
  %t13726 = bitcast i8* %t13725 to i64*
  store i64 49, i64* %t13726, align 4
  %t13730 = getelementptr inbounds i8, i8* %stackTop.133, i64 144
  %t13732 = load i8, i8* %t13730, align 1
  %switch2537 = icmp eq i8 %t13732, 0
  %t13648 = getelementptr inbounds i8, i8* %stackTop.133, i64 176
  %t13649 = bitcast i8* %t13648 to i64*
  br i1 %switch2537, label %L_893, label %L_1619

L_891:                                            ; preds = %L_1702, %L_889
  %stackTop.134 = phi i8* [ %t4570, %L_889 ], [ %t13748, %L_1702 ]
  %frontier.132 = phi i8* [ %t4585, %L_889 ], [ %frontier.0, %L_1702 ]
  %t13734 = getelementptr inbounds i8, i8* %stackTop.134, i64 160
  %t13735 = bitcast i8* %t13734 to i8**
  %142 = bitcast i8* %t13734 to i64**
  %t137361836 = load i64*, i64** %142, align 8
  %t13740 = getelementptr inbounds i8, i8* %stackTop.134, i64 168
  %t13741 = bitcast i8* %t13740 to i64*
  %t13742 = load i64, i64* %t13741, align 4
  store i64 %t13742, i64* %t137361836, align 4
  %t13746 = load i8*, i8** %t13735, align 8
  br label %L_892

L_1702:                                           ; preds = %doSwitchNextBlock
  %t13748 = getelementptr inbounds i8, i8* %stackTop.0, i64 -176
  %t13750 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t13751 = bitcast i8* %t13750 to i64*
  %t13754 = bitcast i8* %stackTop.0 to i64*
  %t13755 = load i64, i64* %t13754, align 4
  store i64 %t13755, i64* %t13751, align 4
  %t13758 = load i8*, i8** %t14203, align 8
  %t13760.not = icmp ult i8* %t13758, %frontier.0
  br i1 %t13760.not, label %L_889, label %L_891

L_887:                                            ; preds = %L_885
  %t13764 = getelementptr inbounds i8, i8* %frontier.133, i64 8
  %t13766 = getelementptr inbounds i8, i8* %t4556.pn, i64 -8
  %t13767 = bitcast i8* %t13766 to i8**
  store i8* %t13764, i8** %t13767, align 8
  %t13775 = bitcast i8* %frontier.133 to i64*
  store i64 41, i64* %t13775, align 4
  %t13777 = getelementptr inbounds i8, i8* %frontier.133, i64 16
  %143 = bitcast i8* %t13766 to i64**
  %t137811834 = load i64*, i64** %143, align 8
  store i64 0, i64* %t137811834, align 4
  %t13785 = getelementptr inbounds i8, i8* %t4556.pn, i64 -160
  %t13786 = bitcast i8* %t13785 to i64*
  store i64 51, i64* %t13786, align 4
  %t13788 = getelementptr inbounds i8, i8* %t4556.pn, i64 8
  %t13789 = bitcast i8* %t13788 to i8**
  store i8* inttoptr (i64 3 to i8*), i8** %t13789, align 8
  %t13792 = getelementptr inbounds i8, i8* %t4556.pn, i64 16
  %t13793 = bitcast i8* %t13792 to i32*
  store i32 0, i32* %t13793, align 4
  %t13796 = bitcast i8* %t4556.pn to i64*
  store i64 75, i64* %t13796, align 4
  store i8* %t13777, i8** %t15197, align 8
  store i8* %t13788, i8** %t15200, align 8
  br label %common.ret

L_885:                                            ; preds = %L_881, %L_883
  %t4556.pn = phi i8* [ %t4556, %L_883 ], [ %stackTop.0, %L_881 ]
  %frontier.133 = phi i8* [ %t4553, %L_883 ], [ %frontier.0, %L_881 ]
  %t13811 = getelementptr inbounds i8, i8* %t4556.pn, i64 -20
  %t13812 = bitcast i8* %t13811 to i32*
  %t13813 = load i32, i32* %t13812, align 4
  %t13814 = tail call i32 @Posix_FileSys_ST_isReg(i32 %t13813)
  %t13807.not = icmp eq i32 %t13814, 0
  br i1 %t13807.not, label %L_1626, label %L_887

L_881:                                            ; preds = %L_1701
  %t13818 = add i32 %t13833, -1
  store i32 %t13818, i32* %t13969, align 4
  %t13824 = load i8*, i8** %t14203, align 8
  %t13826.not = icmp ult i8* %t13824, %frontier.0
  br i1 %t13826.not, label %L_883, label %L_885

L_1701:                                           ; preds = %doSwitchNextBlock
  %t13833 = load i32, i32* %t13969, align 4
  %t13835.not = icmp eq i32 %t13833, 0
  br i1 %t13835.not, label %L_786, label %L_881

L_1700:                                           ; preds = %doSwitchNextBlock
  %t13857 = tail call i64 @Posix_FileSys_Stat_getCTime()
  %t13841 = bitcast i8* %stackTop.0 to i64*
  store i64 %t13857, i64* %t13841, align 4
  %t13844 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  %t13845 = bitcast i8* %t13844 to i8**
  %t13847 = getelementptr inbounds i8, i8* %stackTop.0, i64 -56
  %t13848 = bitcast i8* %t13847 to i8**
  %t13849 = load i8*, i8** %t13848, align 8
  store i8* %t13849, i8** %t13845, align 8
  %t13851 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  br label %L_597.sink.split

L_1699:                                           ; preds = %doSwitchNextBlock
  %t13877 = tail call i64 @Posix_FileSys_Stat_getMTime()
  %t13861 = bitcast i8* %stackTop.0 to i64*
  store i64 %t13877, i64* %t13861, align 4
  %t13864 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  %t13865 = bitcast i8* %t13864 to i8**
  %t13867 = getelementptr inbounds i8, i8* %stackTop.0, i64 -56
  %t13868 = bitcast i8* %t13867 to i8**
  %t13869 = load i8*, i8** %t13868, align 8
  store i8* %t13869, i8** %t13865, align 8
  %t13871 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  br label %L_597.sink.split

L_871:                                            ; preds = %L_861
  %t13901 = tail call i32 @Posix_FileSys_Stat_getUId()
  %t13900 = tail call i32 @Posix_FileSys_Stat_getGId()
  %t13899 = tail call i64 @Posix_FileSys_Stat_getSize()
  %t13898 = tail call i64 @Posix_FileSys_Stat_getATime()
  %t13880 = getelementptr inbounds i8, i8* %stackTop.136, i64 8
  %t13881 = bitcast i8* %t13880 to i64*
  store i64 49, i64* %t13881, align 4
  %t13883 = getelementptr inbounds i8, i8* %stackTop.136, i64 168
  %t13884 = bitcast i8* %t13883 to i64*
  store i64 %t13898, i64* %t13884, align 4
  %t13887 = getelementptr inbounds i8, i8* %stackTop.136, i64 176
  %t13888 = bitcast i8* %t13887 to i8**
  %t13890 = getelementptr inbounds i8, i8* %stackTop.136, i64 112
  %t13891 = bitcast i8* %t13890 to i8**
  %t13892 = load i8*, i8** %t13891, align 8
  store i8* %t13892, i8** %t13888, align 8
  %t13894 = getelementptr inbounds i8, i8* %stackTop.136, i64 160
  br label %L_597.sink.split

L_867:                                            ; preds = %L_866
  %t13912 = add i32 %t13919, -1
  store i32 %t13912, i32* %t13969, align 4
  br label %L_786

L_866:                                            ; preds = %L_861, %doSwitchNextBlock
  %t13919 = load i32, i32* %t13969, align 4
  %t13921.not = icmp eq i32 %t13919, 0
  br i1 %t13921.not, label %L_786, label %L_867

L_861:                                            ; preds = %L_855
  %t13941 = tail call i64 @Posix_FileSys_Stat_getDev()
  %t13940 = tail call i64 @Posix_FileSys_Stat_getINo()
  %t13939 = tail call i32 @Posix_FileSys_Stat_getMode()
  %t13936 = getelementptr inbounds i8, i8* %stackTop.136, i64 148
  %t13937 = bitcast i8* %t13936 to i32*
  store i32 %t13939, i32* %t13937, align 4
  %t13938 = tail call i64 @Posix_FileSys_Stat_getNLink()
  %144 = icmp ult i64 %t13938, 2147483648
  br i1 %144, label %L_871, label %L_866

L_855:                                            ; preds = %L_851, %L_853
  %stackTop.136 = phi i8* [ %t4283, %L_853 ], [ %stackTop.137, %L_851 ]
  %frontier.134 = phi i8* [ %t4298, %L_853 ], [ %t14151, %L_851 ]
  %t13986 = getelementptr inbounds i8, i8* %frontier.134, i64 8
  %t13988 = getelementptr inbounds i8, i8* %stackTop.136, i64 136
  %t13989 = bitcast i8* %t13988 to i8**
  store i8* %t13986, i8** %t13989, align 8
  %t13997 = bitcast i8* %frontier.134 to i64*
  store i64 27, i64* %t13997, align 4
  %145 = bitcast i8* %t13988 to i8***
  %t140031770 = load i8**, i8*** %145, align 8
  store i8* inttoptr (i64 1 to i8*), i8** %t140031770, align 8
  %t14008 = getelementptr inbounds i8, i8* %stackTop.136, i64 96
  %t14009 = bitcast i8* %t14008 to i8**
  %146 = bitcast i8* %t14008 to i8***
  %t140101771 = load i8**, i8*** %146, align 8
  %t14013 = load i8*, i8** %t140101771, align 8
  %t14015 = getelementptr inbounds i8, i8* %frontier.134, i64 24
  %t14019 = getelementptr inbounds i8, i8* %frontier.134, i64 16
  %t14020 = bitcast i8* %t14019 to i64*
  store i64 103, i64* %t14020, align 4
  %t14025 = bitcast i8* %t14015 to i8**
  %t14029 = load i8*, i8** %t13989, align 8
  store i8* %t14029, i8** %t14025, align 8
  %t14031 = getelementptr inbounds i8, i8* %frontier.134, i64 40
  %t14035 = getelementptr inbounds i8, i8* %frontier.134, i64 32
  %t14036 = bitcast i8* %t14035 to i64*
  store i64 133, i64* %t14036, align 4
  %t14041 = bitcast i8* %t14031 to i8**
  store i8* %t14015, i8** %t14041, align 8
  %t14045 = getelementptr inbounds i8, i8* %frontier.134, i64 48
  %t14046 = bitcast i8* %t14045 to i8**
  store i8* %t14013, i8** %t14046, align 8
  %t14051 = load i8*, i8** %t14009, align 8
  %t14052 = ptrtoint i8* %t14051 to i64
  %t14054 = lshr i64 %t14052, 8
  %t14057 = load i8*, i8** %t14171, align 8
  %t14060 = getelementptr inbounds i8, i8* %t14057, i64 %t14054
  store i8 1, i8* %t14060, align 1
  %t140661772 = load i8**, i8*** %146, align 8
  store i8* %t14031, i8** %t140661772, align 8
  %t140731773 = load i8**, i8*** %146, align 8
  %t14076 = load i8*, i8** %t140731773, align 8
  %t14078 = getelementptr inbounds i8, i8* %frontier.134, i64 64
  %t14082 = getelementptr inbounds i8, i8* %frontier.134, i64 56
  %t14083 = bitcast i8* %t14082 to i64*
  store i64 133, i64* %t14083, align 4
  %t14088 = bitcast i8* %t14078 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t14088, align 8
  %t14091 = getelementptr inbounds i8, i8* %frontier.134, i64 72
  %t14092 = bitcast i8* %t14091 to i8**
  store i8* %t14076, i8** %t14092, align 8
  %t14097 = load i8*, i8** %t14009, align 8
  %t14098 = ptrtoint i8* %t14097 to i64
  %t14100 = lshr i64 %t14098, 8
  %t14103 = load i8*, i8** %t14171, align 8
  %t14106 = getelementptr inbounds i8, i8* %t14103, i64 %t14100
  store i8 1, i8* %t14106, align 1
  %t141121774 = load i8**, i8*** %146, align 8
  store i8* %t14078, i8** %t141121774, align 8
  %t14116 = tail call i32 @Posix_ProcEnv_isatty(i32 1)
  %t13982.not = icmp eq i32 %t14116, 0
  %.sink = zext i1 %t13982.not to i8
  %147 = getelementptr inbounds i8, i8* %stackTop.136, i64 144
  store i8 %.sink, i8* %147, align 1
  %t13948 = getelementptr inbounds i8, i8* %frontier.134, i64 88
  %t13950 = getelementptr inbounds i8, i8* %stackTop.136, i64 152
  %t13951 = bitcast i8* %t13950 to i8**
  store i8* %t13948, i8** %t13951, align 8
  %t13958 = getelementptr inbounds i8, i8* %frontier.134, i64 80
  %t13959 = bitcast i8* %t13958 to i64*
  store i64 35, i64* %t13959, align 4
  %t13961 = getelementptr inbounds i8, i8* %frontier.134, i64 96
  %148 = bitcast i8* %t13950 to i32**
  %t139651776 = load i32*, i32** %148, align 8
  store i32 0, i32* %t139651776, align 4
  %t13970 = load i32, i32* %t13969, align 4
  %t13971 = add i32 %t13970, 1
  store i32 %t13971, i32* %t13969, align 4
  %t13975 = tail call i32 @Posix_FileSys_Stat_fstat(i32 1)
  %t13944.not = icmp eq i32 %t13975, -1
  br i1 %t13944.not, label %L_1629, label %L_861

L_851:                                            ; preds = %L_1697, %L_849
  %stackTop.137 = phi i8* [ %t4260, %L_849 ], [ %t14194, %L_1697 ]
  %frontier.135 = phi i8* [ %t4275, %L_849 ], [ %frontier.0, %L_1697 ]
  %t14137 = getelementptr inbounds i8, i8* %stackTop.137, i64 56
  %t14138 = bitcast i8* %t14137 to i8**
  %149 = bitcast i8* %t14137 to i8***
  %t141391767 = load i8**, i8*** %149, align 8
  %t14142 = load i8*, i8** %t141391767, align 8
  %t14144 = getelementptr inbounds i8, i8* %frontier.135, i64 8
  %t14149 = bitcast i8* %frontier.135 to i64*
  store i64 113, i64* %t14149, align 4
  %t14151 = getelementptr inbounds i8, i8* %frontier.135, i64 24
  %t14154 = bitcast i8* %t14144 to i8**
  %t14158 = load i8*, i8** %t14138, align 8
  store i8* %t14158, i8** %t14154, align 8
  %t14160 = getelementptr inbounds i8, i8* %frontier.135, i64 16
  %t14161 = bitcast i8* %t14160 to i8**
  store i8* %t14142, i8** %t14161, align 8
  %t14166 = load i8*, i8** %t14138, align 8
  %t14167 = ptrtoint i8* %t14166 to i64
  %t14169 = lshr i64 %t14167, 8
  %t14172 = load i8*, i8** %t14171, align 8
  %t14175 = getelementptr inbounds i8, i8* %t14172, i64 %t14169
  store i8 1, i8* %t14175, align 1
  %t141811768 = load i8**, i8*** %149, align 8
  store i8* %t14144, i8** %t141811768, align 8
  %t14127 = load i8*, i8** %t14203, align 8
  %t14129.not = icmp ult i8* %t14127, %t14151
  br i1 %t14129.not, label %L_853, label %L_855

L_1697:                                           ; preds = %doSwitchNextBlock
  %t14194 = getelementptr inbounds i8, i8* %stackTop.0, i64 -136
  %t14196 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t14197 = bitcast i8* %t14196 to i8**
  %t14200 = bitcast i8* %stackTop.0 to i8**
  %t14201 = load i8*, i8** %t14200, align 8
  store i8* %t14201, i8** %t14197, align 8
  %t14204 = load i8*, i8** %t14203, align 8
  %t14206.not = icmp ult i8* %t14204, %frontier.0
  br i1 %t14206.not, label %L_849, label %L_851

L_1696:                                           ; preds = %doSwitchNextBlock
  %t14212 = getelementptr inbounds i8, i8* %stackTop.0, i64 -88
  %t14213 = bitcast i8* %t14212 to i8**
  %t14216 = bitcast i8* %stackTop.0 to i8**
  %t14217 = load i8*, i8** %t14216, align 8
  store i8* %t14217, i8** %t14213, align 8
  %t14220 = bitcast i8* %stackTop.0 to i64*
  store i64 8, i64* %t14220, align 4
  %t14222 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  %t14223 = bitcast i8* %t14222 to i8**
  %t14225 = getelementptr inbounds i8, i8* %stackTop.0, i64 -16
  %t14226 = bitcast i8* %t14225 to i8**
  %t14227 = load i8*, i8** %t14226, align 8
  store i8* %t14227, i8** %t14223, align 8
  %t14229 = getelementptr inbounds i8, i8* %stackTop.0, i64 16
  %t14230 = bitcast i8* %t14229 to i8**
  %t14232 = getelementptr inbounds i8, i8* %stackTop.0, i64 -24
  %t14233 = bitcast i8* %t14232 to i8**
  %t14234 = load i8*, i8** %t14233, align 8
  store i8* %t14234, i8** %t14230, align 8
  %t14236 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  br label %L_638.sink.split

L_1695:                                           ; preds = %doSwitchNextBlock
  %t14243 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t14244 = bitcast i8* %t14243 to i8**
  %t14246 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  %t14247 = bitcast i8* %t14246 to i8**
  %t14248 = load i8*, i8** %t14247, align 8
  store i8* %t14248, i8** %t14244, align 8
  store i8* inttoptr (i64 2000000001 to i8*), i8** %t14247, align 8
  %t14254 = getelementptr inbounds i8, i8* %stackTop.0, i64 16
  %t14255 = bitcast i8* %t14254 to i64*
  store i64 8, i64* %t14255, align 4
  %t14257 = getelementptr inbounds i8, i8* %stackTop.0, i64 24
  %t14258 = bitcast i8* %t14257 to i8**
  %t14260 = getelementptr inbounds i8, i8* %stackTop.0, i64 -80
  %t14261 = bitcast i8* %t14260 to i8**
  %t14262 = load i8*, i8** %t14261, align 8
  store i8* %t14262, i8** %t14258, align 8
  br label %L_749.sink.split

L_1694:                                           ; preds = %doSwitchNextBlock
  %t14271 = getelementptr inbounds i8, i8* %stackTop.0, i64 -8
  %t14272 = bitcast i8* %t14271 to i8**
  %t14274 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  %t14275 = bitcast i8* %t14274 to i8**
  %t14276 = load i8*, i8** %t14275, align 8
  store i8* %t14276, i8** %t14272, align 8
  store i8* inttoptr (i64 2000001 to i8*), i8** %t14275, align 8
  %t14282 = getelementptr inbounds i8, i8* %stackTop.0, i64 16
  %t14283 = bitcast i8* %t14282 to i64*
  store i64 8, i64* %t14283, align 4
  %t14285 = getelementptr inbounds i8, i8* %stackTop.0, i64 24
  %t14286 = bitcast i8* %t14285 to i8**
  %t14288 = getelementptr inbounds i8, i8* %stackTop.0, i64 -72
  %t14289 = bitcast i8* %t14288 to i8**
  %t14290 = load i8*, i8** %t14289, align 8
  store i8* %t14290, i8** %t14286, align 8
  br label %L_749.sink.split

L_844:                                            ; preds = %L_839, %L_842
  %t14299 = phi i8* [ %t14299.pre, %L_842 ], [ %t14522, %L_839 ]
  %t4258.pn = phi i8* [ %t4258, %L_842 ], [ %t14528, %L_839 ]
  %frontier.136 = phi i8* [ %t4255, %L_842 ], [ %t14525, %L_839 ]
  %t14300 = getelementptr inbounds i8, i8* %t14299, i64 -8
  %t14301 = bitcast i8* %t14300 to i64*
  store i64 11, i64* %t14301, align 4
  %t14303 = getelementptr inbounds i8, i8* %t4258.pn, i64 -64
  %t14304 = bitcast i8* %t14303 to i8**
  %150 = bitcast i8* %t14303 to i8***
  %t143051695 = load i8**, i8*** %150, align 8
  %t14308 = load i8*, i8** %t143051695, align 8
  %t14310 = getelementptr inbounds i8, i8* %frontier.136, i64 8
  %t14315 = bitcast i8* %frontier.136 to i64*
  store i64 113, i64* %t14315, align 4
  %t14320 = bitcast i8* %t14310 to i8**
  store i8* inttoptr (i64 1 to i8*), i8** %t14320, align 8
  %t14323 = getelementptr inbounds i8, i8* %frontier.136, i64 16
  %t14324 = bitcast i8* %t14323 to i8**
  store i8* %t14308, i8** %t14324, align 8
  %t14329 = load i8*, i8** %t14304, align 8
  %t14330 = ptrtoint i8* %t14329 to i64
  %t14332 = lshr i64 %t14330, 8
  %t14335 = load i8*, i8** %t14171, align 8
  %t14338 = getelementptr inbounds i8, i8* %t14335, i64 %t14332
  store i8 1, i8* %t14338, align 1
  %t143441696 = load i8**, i8*** %150, align 8
  store i8* %t14310, i8** %t143441696, align 8
  %t14349 = getelementptr inbounds i8, i8* %frontier.136, i64 32
  %t14351 = getelementptr inbounds i8, i8* %t4258.pn, i64 -24
  %t14352 = bitcast i8* %t14351 to i8**
  store i8* %t14349, i8** %t14352, align 8
  %t14359 = getelementptr inbounds i8, i8* %frontier.136, i64 24
  %t14360 = bitcast i8* %t14359 to i64*
  store i64 63, i64* %t14360, align 4
  %151 = bitcast i8* %t14351 to i8***
  %t143661697 = load i8**, i8*** %151, align 8
  store i8* inttoptr (i64 1 to i8*), i8** %t143661697, align 8
  %t143731698 = load i8**, i8*** %150, align 8
  %t14376 = load i8*, i8** %t143731698, align 8
  %t14378 = getelementptr inbounds i8, i8* %frontier.136, i64 48
  %t14382 = getelementptr inbounds i8, i8* %frontier.136, i64 40
  %t14383 = bitcast i8* %t14382 to i64*
  store i64 113, i64* %t14383, align 4
  %t14385 = getelementptr inbounds i8, i8* %frontier.136, i64 64
  %t14388 = bitcast i8* %t14378 to i8**
  store i8* inttoptr (i64 2 to i8*), i8** %t14388, align 8
  %t14391 = getelementptr inbounds i8, i8* %frontier.136, i64 56
  %t14392 = bitcast i8* %t14391 to i8**
  store i8* %t14376, i8** %t14392, align 8
  %t14397 = load i8*, i8** %t14304, align 8
  %t14398 = ptrtoint i8* %t14397 to i64
  %t14400 = lshr i64 %t14398, 8
  %t14403 = load i8*, i8** %t14171, align 8
  %t14406 = getelementptr inbounds i8, i8* %t14403, i64 %t14400
  store i8 1, i8* %t14406, align 1
  %t144121699 = load i8**, i8*** %150, align 8
  store i8* %t14378, i8** %t144121699, align 8
  %t14417 = getelementptr inbounds i8, i8* %t4258.pn, i64 -112
  %t14418 = bitcast i8* %t14417 to i64*
  store i64 51, i64* %t14418, align 4
  %t14420 = getelementptr inbounds i8, i8* %t4258.pn, i64 -104
  %t14424 = load i8*, i8** %t14423, align 8
  %t14425 = ptrtoint i8* %t14420 to i64
  %t14426 = ptrtoint i8* %t14424 to i64
  %t14427 = sub i64 %t14425, %t14426
  store i64 %t14427, i64* %t15180, align 4
  %t14432 = bitcast i8* %t4258.pn to i8**
  store i8* inttoptr (i64 3 to i8*), i8** %t14432, align 8
  %t14435 = getelementptr inbounds i8, i8* %t4258.pn, i64 8
  %t14436 = bitcast i8* %t14435 to i64*
  store i64 8, i64* %t14436, align 4
  %t14438 = getelementptr inbounds i8, i8* %t4258.pn, i64 16
  %t14439 = bitcast i8* %t14438 to i8**
  %t14441 = getelementptr inbounds i8, i8* %t4258.pn, i64 -72
  %t14442 = bitcast i8* %t14441 to i8**
  %t14443 = load i8*, i8** %t14442, align 8
  store i8* %t14443, i8** %t14439, align 8
  %t14445 = getelementptr inbounds i8, i8* %t4258.pn, i64 -8
  br label %L_749.sink.split

L_839:                                            ; preds = %L_832, %L_837
  %t4229.pn = phi i8* [ %t4229, %L_837 ], [ %t14584, %L_832 ]
  %frontier.137 = phi i8* [ %t4226, %L_837 ], [ %t14581, %L_832 ]
  %t14477 = getelementptr inbounds i8, i8* %frontier.137, i64 8
  %t14479 = getelementptr inbounds i8, i8* %t4229.pn, i64 -8
  %t14480 = bitcast i8* %t14479 to i8**
  store i8* %t14477, i8** %t14480, align 8
  %t14488 = bitcast i8* %frontier.137 to i64*
  store i64 61, i64* %t14488, align 4
  %t14490 = getelementptr inbounds i8, i8* %frontier.137, i64 24
  %152 = bitcast i8* %t14479 to i8***
  %t144941694 = load i8**, i8*** %152, align 8
  %t14498 = getelementptr inbounds i8, i8* %t4229.pn, i64 -16
  %t14499 = bitcast i8* %t14498 to i8**
  %t14500 = load i8*, i8** %t14499, align 8
  store i8* %t14500, i8** %t144941694, align 8
  %t14504 = load i8*, i8** %t14480, align 8
  %t14505 = getelementptr inbounds i8, i8* %t14504, i64 8
  %t14506 = bitcast i8* %t14505 to i8**
  %t14508 = getelementptr inbounds i8, i8* %t4229.pn, i64 -48
  %t14509 = bitcast i8* %t14508 to i8**
  %t14510 = load i8*, i8** %t14509, align 8
  store i8* %t14510, i8** %t14506, align 8
  %t14513 = bitcast i8* %t4229.pn to i64*
  store i64 91, i64* %t14513, align 4
  %t14515 = getelementptr inbounds i8, i8* %t4229.pn, i64 8
  store i8* %t14490, i8** %t15197, align 8
  store i8* %t14515, i8** %t15200, align 8
  %t14522 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 1, i64 21)
  %t14525 = load i8*, i8** %t15197, align 8
  %t14528 = load i8*, i8** %t15200, align 8
  %t14473 = getelementptr inbounds i8, i8* %t14528, i64 -40
  %t14474 = bitcast i8* %t14473 to i8**
  %t14475 = load i8*, i8** %t14474, align 8
  store i8 0, i8* %t14522, align 1
  %t14450 = getelementptr inbounds i8, i8* %t14528, i64 -56
  %t14451 = bitcast i8* %t14450 to i8**
  store i8* %t14522, i8** %t14451, align 8
  store i8* %t14475, i8** %t14474, align 8
  %t14459 = load i8*, i8** %t14203, align 8
  %t14461.not = icmp ult i8* %t14459, %t14525
  br i1 %t14461.not, label %L_842, label %L_844

L_832:                                            ; preds = %L_827, %L_830
  %t4191.pn = phi i8* [ %t4191, %L_830 ], [ %t14727, %L_827 ]
  %frontier.138 = phi i8* [ %t4188, %L_830 ], [ %t14724, %L_827 ]
  %t14601 = getelementptr inbounds i8, i8* %frontier.138, i64 8
  %t14603 = getelementptr inbounds i8, i8* %t4191.pn, i64 -8
  %t14604 = bitcast i8* %t14603 to i8**
  store i8* %t14601, i8** %t14604, align 8
  %t14612 = bitcast i8* %frontier.138 to i64*
  store i64 35, i64* %t14612, align 4
  %t14614 = getelementptr inbounds i8, i8* %frontier.138, i64 16
  %153 = bitcast i8* %t14603 to i32**
  %t146181693 = load i32*, i32** %153, align 8
  store i32 0, i32* %t146181693, align 4
  %t14623 = bitcast i8* %t4191.pn to i64*
  store i64 90, i64* %t14623, align 4
  %t14625 = getelementptr inbounds i8, i8* %t4191.pn, i64 8
  store i8* %t14614, i8** %t15197, align 8
  store i8* %t14625, i8** %t15200, align 8
  %t14632 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 32, i64 21)
  %t14635 = load i8*, i8** %t15197, align 8
  %t14638 = load i8*, i8** %t15200, align 8
  %t14593 = getelementptr inbounds i8, i8* %t14638, i64 -48
  %t14594 = bitcast i8* %t14593 to i8**
  %t14595 = load i8*, i8** %t14594, align 8
  %t14597 = getelementptr inbounds i8, i8* %t14638, i64 -32
  %t14598 = bitcast i8* %t14597 to i8**
  %t14599 = load i8*, i8** %t14598, align 8
  call void @llvm.memset.p0i8.i64(i8* noundef nonnull align 1 dereferenceable(32) %t14632, i8 0, i64 32, i1 false)
  store i8* %t14595, i8** %t14594, align 8
  store i8* %t14599, i8** %t14598, align 8
  %t14568 = getelementptr inbounds i8, i8* %t14638, i64 -8
  %t14569 = bitcast i8* %t14568 to i64*
  store i64 90, i64* %t14569, align 4
  store i8* %t14635, i8** %t15197, align 8
  store i8* %t14638, i8** %t15200, align 8
  %t14578 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 64, i64 21)
  %t14581 = load i8*, i8** %t15197, align 8
  %t14584 = load i8*, i8** %t15200, align 8
  %t14552 = getelementptr inbounds i8, i8* %t14584, i64 -48
  %t14553 = bitcast i8* %t14552 to i8**
  %t14554 = load i8*, i8** %t14553, align 8
  %t14556 = getelementptr inbounds i8, i8* %t14584, i64 -32
  %t14557 = bitcast i8* %t14556 to i8**
  %t14558 = load i8*, i8** %t14557, align 8
  call void @llvm.memset.p0i8.i64(i8* noundef nonnull align 1 dereferenceable(64) %t14578, i8 0, i64 64, i1 false)
  store i8* %t14554, i8** %t14553, align 8
  store i8* %t14558, i8** %t14557, align 8
  %t14539 = load i8*, i8** %t14203, align 8
  %t14541.not = icmp ult i8* %t14539, %t14581
  br i1 %t14541.not, label %L_837, label %L_839

L_827:                                            ; preds = %L_1688, %L_825
  %stackTop.141 = phi i8* [ %t4144, %L_825 ], [ %t14748, %L_1688 ]
  %frontier.139 = phi i8* [ %t4159, %L_825 ], [ %t14773, %L_1688 ]
  %t14667 = getelementptr inbounds i8, i8* %frontier.139, i64 8
  %t14672 = bitcast i8* %frontier.139 to i64*
  store i64 35, i64* %t14672, align 4
  %t14677 = bitcast i8* %t14667 to i32*
  store i32 0, i32* %t14677, align 4
  %t14679 = getelementptr inbounds i8, i8* %frontier.139, i64 24
  %t14681 = getelementptr inbounds i8, i8* %stackTop.141, i64 88
  %t14682 = bitcast i8* %t14681 to i8**
  store i8* %t14679, i8** %t14682, align 8
  %t14689 = getelementptr inbounds i8, i8* %frontier.139, i64 16
  %t14690 = bitcast i8* %t14689 to i64*
  store i64 61, i64* %t14690, align 4
  %t14692 = getelementptr inbounds i8, i8* %frontier.139, i64 40
  %154 = bitcast i8* %t14681 to i8***
  %t146961692 = load i8**, i8*** %154, align 8
  store i8* %t14667, i8** %t146961692, align 8
  %t14703 = load i8*, i8** %t14682, align 8
  %t14704 = getelementptr inbounds i8, i8* %t14703, i64 8
  %t14705 = bitcast i8* %t14704 to i8**
  %t14707 = getelementptr inbounds i8, i8* %stackTop.141, i64 64
  %t14708 = bitcast i8* %t14707 to i8**
  %t14709 = load i8*, i8** %t14708, align 8
  store i8* %t14709, i8** %t14705, align 8
  %t14711 = getelementptr inbounds i8, i8* %stackTop.141, i64 96
  %t14712 = bitcast i8* %t14711 to i64*
  store i64 89, i64* %t14712, align 4
  %t14714 = getelementptr inbounds i8, i8* %stackTop.141, i64 104
  store i8* %t14692, i8** %t15197, align 8
  store i8* %t14714, i8** %t15200, align 8
  %t14721 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 65, i64 21)
  %t14724 = load i8*, i8** %t15197, align 8
  %t14727 = load i8*, i8** %t15200, align 8
  %t14663 = getelementptr inbounds i8, i8* %t14727, i64 -24
  %t14664 = bitcast i8* %t14663 to i8**
  %t14665 = load i8*, i8** %t14664, align 8
  call void @llvm.memset.p0i8.i64(i8* noundef nonnull align 1 dereferenceable(65) %t14721, i8 0, i64 65, i1 false)
  %t14640 = getelementptr inbounds i8, i8* %t14727, i64 -40
  %t14641 = bitcast i8* %t14640 to i8**
  store i8* %t14721, i8** %t14641, align 8
  store i8* %t14665, i8** %t14664, align 8
  %t14649 = load i8*, i8** %t14203, align 8
  %t14651.not = icmp ult i8* %t14649, %t14724
  br i1 %t14651.not, label %L_830, label %L_832

L_1688:                                           ; preds = %L_1664
  store i8* %t14790, i8** %t14789, align 8
  %t14761 = bitcast i8* %t14812 to i64*
  store i64 88, i64* %t14761, align 4
  %t14763 = getelementptr inbounds i8, i8* %t14812, i64 8
  store i8* %t14809, i8** %t15197, align 8
  store i8* %t14763, i8** %t15200, align 8
  %t14770 = tail call i8* @GC_sequenceAllocate(i8* %gcState, i64 0, i64 33, i64 21)
  %t14773 = load i8*, i8** %t15197, align 8
  %t14776 = load i8*, i8** %t15200, align 8
  %t14752 = getelementptr inbounds i8, i8* %t14776, i64 -24
  %t14753 = bitcast i8* %t14752 to i8**
  %t14754 = load i8*, i8** %t14753, align 8
  call void @llvm.memset.p0i8.i64(i8* noundef nonnull align 1 dereferenceable(33) %t14770, i8 0, i64 33, i1 false)
  %t14748 = getelementptr inbounds i8, i8* %t14776, i64 -88
  store i8* %t14770, i8** %t14753, align 8
  %t14733 = getelementptr inbounds i8, i8* %t14776, i64 -8
  %t14734 = bitcast i8* %t14733 to i8**
  store i8* %t14754, i8** %t14734, align 8
  %t14738 = load i8*, i8** %t14203, align 8
  %t14740.not = icmp ult i8* %t14738, %t14773
  br i1 %t14740.not, label %L_825, label %L_827

L_815:                                            ; preds = %L_1685, %L_812, %L_1679, %L_1681
  %stackTop.142 = phi i8* [ %stackTop.145, %L_1681 ], [ %t3998, %L_812 ], [ %stackTop.143.ph, %L_1685 ], [ %t14970, %L_1679 ]
  %frontier.140 = phi i8* [ %frontier.143, %L_1681 ], [ %t4013, %L_812 ], [ %frontier.141.ph, %L_1685 ], [ %t14995, %L_1679 ]
  %t14854 = getelementptr inbounds i8, i8* %stackTop.142, i64 40
  %t14855 = bitcast i8* %t14854 to i8**
  %t14856 = load i8*, i8** %t14855, align 8
  %t14857 = getelementptr inbounds i8, i8* %t14856, i64 -8
  %t14858 = bitcast i8* %t14857 to i64*
  store i64 17, i64* %t14858, align 4
  %t14860 = getelementptr inbounds i8, i8* %frontier.140, i64 8
  %t14862 = getelementptr inbounds i8, i8* %stackTop.142, i64 48
  %t14863 = bitcast i8* %t14862 to i8**
  store i8* %t14860, i8** %t14863, align 8
  %t14871 = bitcast i8* %frontier.140 to i64*
  store i64 55, i64* %t14871, align 4
  %155 = bitcast i8* %t14862 to i64**
  %t148771690 = load i64*, i64** %155, align 8
  store i64 8, i64* %t148771690, align 4
  %t14883 = load i8*, i8** %t14863, align 8
  %t14884 = getelementptr inbounds i8, i8* %t14883, i64 8
  %t14885 = bitcast i8* %t14884 to i8**
  %t14889 = load i8*, i8** %t14855, align 8
  store i8* %t14889, i8** %t14885, align 8
  %t14892 = getelementptr inbounds i8, i8* %frontier.140, i64 32
  %t14894 = getelementptr inbounds i8, i8* %stackTop.142, i64 56
  %t14895 = bitcast i8* %t14894 to i8**
  store i8* %t14892, i8** %t14895, align 8
  %t14902 = getelementptr inbounds i8, i8* %frontier.140, i64 24
  %t14903 = bitcast i8* %t14902 to i64*
  store i64 31, i64* %t14903, align 4
  %156 = bitcast i8* %t14894 to i8***
  %t149091691 = load i8**, i8*** %156, align 8
  store i8* inttoptr (i64 1 to i8*), i8** %t149091691, align 8
  %t14905 = getelementptr inbounds i8, i8* %frontier.140, i64 40
  %t14828 = getelementptr inbounds i8, i8* %stackTop.142, i64 64
  %t14829 = bitcast i8* %t14828 to i64*
  store i64 86, i64* %t14829, align 4
  %t14831 = getelementptr inbounds i8, i8* %stackTop.142, i64 72
  store i8* %t14905, i8** %t15197, align 8
  store i8* %t14831, i8** %t15200, align 8
  %t14838 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 0, i64 256, i64 57)
  %t14841 = load i8*, i8** %t15197, align 8
  %t14844 = load i8*, i8** %t15200, align 8
  br label %L_1672

L_1685:                                           ; preds = %L_809
  %t14914 = getelementptr inbounds i8, i8* %stackTop.143.ph, i64 40
  %t14915 = bitcast i8* %t14914 to i8**
  store i8* %TP_2.7.ph, i8** %t14915, align 8
  %t14919 = load i8*, i8** %t14203, align 8
  %t14921.not = icmp ult i8* %t14919, %frontier.141.ph
  br i1 %t14921.not, label %L_812, label %L_815

L_809:                                            ; preds = %L_809, %L_809.preheader
  %TP_1.22.in.in = phi i8* [ %TP_0.72, %L_809 ], [ %TP_1.22.in.in.ph, %L_809.preheader ]
  %TP_0.72.in.in = getelementptr inbounds i8, i8* %TP_1.22.in.in, i64 8
  %TP_0.72.in = bitcast i8* %TP_0.72.in.in to i8**
  %TP_0.72 = load i8*, i8** %TP_0.72.in, align 8
  %TP_1.22.in = bitcast i8* %TP_1.22.in.in to i8**
  %TP_1.22 = load i8*, i8** %TP_1.22.in, align 8
  %t14936 = bitcast i8* %TP_1.22 to i64*
  %t14937 = load i64, i64* %t14936, align 4
  %t14939 = getelementptr inbounds i8, i8* %TP_1.22, i64 8
  %t14940 = bitcast i8* %t14939 to i64*
  %t14941 = load i64, i64* %t14940, align 4
  %t14944 = shl nsw i64 %t14941, 3
  %t14945 = getelementptr inbounds i8, i8* %TP_2.7.ph, i64 %t14944
  %t14947 = bitcast i8* %t14945 to i64*
  store i64 %t14937, i64* %t14947, align 4
  %cond58 = icmp eq i8* %TP_0.72, inttoptr (i64 1 to i8*)
  br i1 %cond58, label %L_1685, label %L_809

L_809.preheader:                                  ; preds = %L_806, %L_zeroLen_17
  %TP_2.7.ph = phi i8* [ getelementptr (i8, i8* @staticHeapM, i64 40), %L_zeroLen_17 ], [ %t14992, %L_806 ]
  %TP_1.22.in.in.ph = phi i8* [ %TP_0.75, %L_zeroLen_17 ], [ %t14975, %L_806 ]
  %stackTop.143.ph = phi i8* [ %stackTop.145, %L_zeroLen_17 ], [ %t14970, %L_806 ]
  %frontier.141.ph = phi i8* [ %frontier.143, %L_zeroLen_17 ], [ %t14995, %L_806 ]
  br label %L_809

L_806:                                            ; preds = %L_nonZeroLen_9, %L_804
  %t14980 = phi i64 [ %t14980.pre, %L_804 ], [ %TW64_0.42, %L_nonZeroLen_9 ]
  %stackTop.144 = phi i8* [ %t3974, %L_804 ], [ %stackTop.145, %L_nonZeroLen_9 ]
  %frontier.142 = phi i8* [ %t3989, %L_804 ], [ %frontier.143, %L_nonZeroLen_9 ]
  %t14982 = getelementptr inbounds i8, i8* %stackTop.144, i64 56
  %t14983 = bitcast i8* %t14982 to i64*
  store i64 85, i64* %t14983, align 4
  %t14985 = getelementptr inbounds i8, i8* %stackTop.144, i64 64
  store i8* %frontier.142, i8** %t15197, align 8
  store i8* %t14985, i8** %t15200, align 8
  %t14992 = tail call i8* @GC_sequenceAllocate(i8* nonnull %gcState, i64 40, i64 %t14980, i64 53)
  %t14995 = load i8*, i8** %t15197, align 8
  %t14998 = load i8*, i8** %t15200, align 8
  %t14970 = getelementptr inbounds i8, i8* %t14998, i64 -64
  %t14973 = getelementptr inbounds i8, i8* %t14998, i64 -16
  %t14974 = bitcast i8* %t14973 to i8**
  %t14975 = load i8*, i8** %t14974, align 8
  %cond60 = icmp eq i8* %t14975, inttoptr (i64 1 to i8*)
  br i1 %cond60, label %L_1679, label %L_809.preheader

L_nonZeroLen_9:                                   ; preds = %L_1684
  %t15001 = load i8*, i8** %t14203, align 8
  %t15003.not = icmp ult i8* %t15001, %frontier.143
  br i1 %t15003.not, label %L_804, label %L_806

L_1684:                                           ; preds = %L_782
  %t15007 = getelementptr inbounds i8, i8* %stackTop.145, i64 40
  %t15008 = bitcast i8* %t15007 to i64*
  store i64 %TW64_0.42, i64* %t15008, align 4
  %t15011 = getelementptr inbounds i8, i8* %stackTop.145, i64 48
  %t15012 = bitcast i8* %t15011 to i8**
  store i8* %TP_0.75, i8** %t15012, align 8
  %t15018.not = icmp eq i64 %TW64_0.42, 0
  br i1 %t15018.not, label %L_zeroLen_17, label %L_nonZeroLen_9

L_801:                                            ; preds = %L_786
  br label %print_1

L_800:                                            ; preds = %L_786
  br label %print_1

L_799:                                            ; preds = %L_786
  br label %print_1

L_798:                                            ; preds = %L_786
  br label %print_1

L_797:                                            ; preds = %L_786
  br label %print_1

L_794:                                            ; preds = %L_788
  %t15038 = bitcast i8* %TP_0.74 to i8**
  %t15039 = load i8*, i8** %t15038, align 8
  tail call void @Stdio_print(i8* getelementptr (i8, i8* @staticHeapI, i64 4192))
  br label %print_1

L_792:                                            ; preds = %L_788
  br label %print_1

print_1:                                          ; preds = %L_801, %L_800, %L_799, %L_798, %L_797, %L_792, %L_789, %L_786, %L_788, %L_794
  %TP_0.73.sink = phi i8* [ %t15039, %L_794 ], [ getelementptr (i8, i8* @staticHeapI, i64 4256), %L_792 ], [ getelementptr (i8, i8* @staticHeapI, i64 4352), %L_789 ], [ getelementptr (i8, i8* @staticHeapI, i64 4016), %L_801 ], [ getelementptr (i8, i8* @staticHeapI, i64 4056), %L_800 ], [ getelementptr (i8, i8* @staticHeapI, i64 4096), %L_799 ], [ getelementptr (i8, i8* @staticHeapI, i64 4128), %L_798 ], [ getelementptr (i8, i8* @staticHeapI, i64 4160), %L_797 ], [ getelementptr (i8, i8* @staticHeapI, i64 3984), %L_786 ], [ getelementptr (i8, i8* @staticHeapI, i64 4224), %L_788 ]
  tail call void @Stdio_print(i8* %TP_0.73.sink)
  tail call void @Stdio_print(i8* getelementptr (i8, i8* @staticHeapI, i64 3152))
  tail call void @MLton_bug(i8* getelementptr (i8, i8* @staticHeapI, i64 4288))
  %t15050 = tail call i64 @MLton_unreachable()
  br label %common.ret

L_789:                                            ; preds = %L_788
  br label %print_1

L_788:                                            ; preds = %L_786
  %t15059 = getelementptr inbounds i8, i8* %TP_0.74, i64 -8
  %t15060 = bitcast i8* %t15059 to i64*
  %t15061 = load i64, i64* %t15060, align 4
  %t15063 = lshr i64 %t15061, 1
  %trunc = trunc i64 %t15063 to i63
  switch i63 %trunc, label %L_2430 [
    i63 62, label %L_789
    i63 63, label %L_792
    i63 64, label %print_1
    i63 65, label %L_794
  ]

L_2430:                                           ; preds = %L_788
  unreachable

L_786:                                            ; preds = %L_1701, %L_1708, %L_1712, %L_783, %L_866, %L_914, %L_941, %L_1570, %L_1796, %L_1595, %L_1798, %L_1629, %L_1801, %L_867, %L_915, %L_942, %L_1587, %L_1592, %L_1612, %L_1646
  %TP_0.74 = phi i8* [ %t4976, %L_1592 ], [ %t4376, %L_1646 ], [ inttoptr (i64 1 to i8*), %L_867 ], [ %t4810, %L_1612 ], [ inttoptr (i64 1 to i8*), %L_915 ], [ %t5047, %L_1587 ], [ inttoptr (i64 1 to i8*), %L_942 ], [ getelementptr (i8, i8* @staticHeapI, i64 8968), %L_1629 ], [ inttoptr (i64 2 to i8*), %L_1801 ], [ getelementptr (i8, i8* @staticHeapI, i64 8968), %L_1595 ], [ inttoptr (i64 2 to i8*), %L_1798 ], [ getelementptr (i8, i8* @staticHeapI, i64 8968), %L_1570 ], [ inttoptr (i64 2 to i8*), %L_1796 ], [ getelementptr (i8, i8* @staticHeapI, i64 8968), %L_941 ], [ getelementptr (i8, i8* @staticHeapI, i64 8968), %L_914 ], [ getelementptr (i8, i8* @staticHeapI, i64 8968), %L_866 ], [ inttoptr (i64 1 to i8*), %L_783 ], [ getelementptr (i8, i8* @staticHeapI, i64 8968), %L_1712 ], [ getelementptr (i8, i8* @staticHeapI, i64 8968), %L_1708 ], [ getelementptr (i8, i8* @staticHeapI, i64 8968), %L_1701 ]
  tail call void @Stdio_print(i8* getelementptr (i8, i8* @staticHeapI, i64 3904))
  %t15066 = ptrtoint i8* %TP_0.74 to i64
  switch i64 %t15066, label %L_788 [
    i64 1, label %L_797
    i64 2, label %L_798
    i64 3, label %L_799
    i64 5, label %L_800
    i64 6, label %L_801
    i64 7, label %print_1
  ]

L_784:                                            ; preds = %L_783
  %t15108 = add nsw i64 %TW64_0.42, 1
  %t15072 = getelementptr inbounds i8, i8* %frontier.143, i64 8
  %t15077 = bitcast i8* %frontier.143 to i64*
  store i64 29, i64* %t15077, align 4
  %t15082 = bitcast i8* %t15072 to i64*
  store i64 %TW64_1.26, i64* %t15082, align 4
  %t15085 = getelementptr inbounds i8, i8* %frontier.143, i64 16
  %t15086 = bitcast i8* %t15085 to i64*
  store i64 %TW64_0.42, i64* %t15086, align 4
  %t15089 = getelementptr inbounds i8, i8* %frontier.143, i64 32
  %t15093 = getelementptr inbounds i8, i8* %frontier.143, i64 24
  %t15094 = bitcast i8* %t15093 to i64*
  store i64 97, i64* %t15094, align 4
  %t15096 = getelementptr inbounds i8, i8* %frontier.143, i64 48
  %t15099 = bitcast i8* %t15089 to i8**
  store i8* %t15072, i8** %t15099, align 8
  %t15102 = getelementptr inbounds i8, i8* %frontier.143, i64 40
  %t15103 = bitcast i8* %t15102 to i8**
  store i8* %TP_0.75, i8** %t15103, align 8
  br label %loop_24

L_783:                                            ; preds = %L_782
  %t15111.not = icmp eq i64 %TW64_0.42, 9223372036854775807
  br i1 %t15111.not, label %L_786, label %L_784

L_782:                                            ; preds = %loop_24, %L_1803
  %TP_0.75 = phi i8* [ %t3913, %L_1803 ], [ %TP_0.76, %loop_24 ]
  %TW64_0.42 = phi i64 [ %t3909, %L_1803 ], [ %TW64_0.43, %loop_24 ]
  %TW64_1.26 = phi i64 [ %t3905, %L_1803 ], [ %TW64_1.27, %loop_24 ]
  %stackTop.145 = phi i8* [ %t3901, %L_1803 ], [ %stackTop.146, %loop_24 ]
  %frontier.143 = phi i8* [ %t3943, %L_1803 ], [ %frontier.144, %loop_24 ]
  %t15115.not = icmp eq i64 %TW64_1.26, 0
  br i1 %t15115.not, label %L_1684, label %L_783

loop_24:                                          ; preds = %L_779, %L_784
  %TP_0.76 = phi i8* [ getelementptr (i8, i8* @staticHeapI, i64 8920), %L_779 ], [ %t15089, %L_784 ]
  %TW64_0.43 = phi i64 [ 1, %L_779 ], [ %t15108, %L_784 ]
  %TW64_1.27 = phi i64 [ 4611686018427387904, %L_779 ], [ 0, %L_784 ]
  %stackTop.146 = phi i8* [ %stackTop.147, %L_779 ], [ %stackTop.145, %L_784 ]
  %frontier.144 = phi i8* [ %t15160, %L_779 ], [ %t15096, %L_784 ]
  %t15120 = load i8*, i8** %t14203, align 8
  %t15122.not = icmp ult i8* %t15120, %frontier.144
  br i1 %t15122.not, label %L_1803, label %L_782

L_779:                                            ; preds = %L_776, %L_777
  %stackTop.147 = phi i8* [ %t3881, %L_777 ], [ %stackTop.148, %L_776 ]
  %frontier.145 = phi i8* [ %t3896, %L_777 ], [ %frontier.146, %L_776 ]
  %t15126 = getelementptr inbounds i8, i8* %frontier.145, i64 8
  %t15128 = getelementptr inbounds i8, i8* %stackTop.147, i64 24
  %t15129 = bitcast i8* %t15128 to i8**
  store i8* %t15126, i8** %t15129, align 8
  %t15137 = bitcast i8* %frontier.145 to i64*
  store i64 19, i64* %t15137, align 4
  %157 = bitcast i8* %t15128 to i32**
  %t151431688 = load i32*, i32** %157, align 8
  store i32 0, i32* %t151431688, align 4
  %t15147 = getelementptr inbounds i8, i8* %frontier.145, i64 24
  %t15149 = getelementptr inbounds i8, i8* %stackTop.147, i64 32
  %t15150 = bitcast i8* %t15149 to i8**
  store i8* %t15147, i8** %t15150, align 8
  %t15157 = getelementptr inbounds i8, i8* %frontier.145, i64 16
  %t15158 = bitcast i8* %t15157 to i64*
  store i64 19, i64* %t15158, align 4
  %t15160 = getelementptr inbounds i8, i8* %frontier.145, i64 32
  %158 = bitcast i8* %t15149 to i32**
  %t151641689 = load i32*, i32** %158, align 8
  store i32 0, i32* %t151641689, align 4
  br label %loop_24

L_776:                                            ; preds = %L_1682
  %t15171 = load i8*, i8** %t14203, align 8
  %t15173.not = icmp ult i8* %t15171, %frontier.146
  br i1 %t15173.not, label %L_777, label %L_779

L_1682:                                           ; preds = %L_0, %L_1, %doSwitchNextBlock
  %stackTop.148 = phi i8* [ %t15190, %L_1 ], [ %stackTop.0, %L_0 ], [ %stackTop.0, %doSwitchNextBlock ]
  %frontier.146 = phi i8* [ %t15205, %L_1 ], [ %frontier.0, %L_0 ], [ %frontier.0, %doSwitchNextBlock ]
  %t15178 = bitcast i8* %stackTop.148 to i64*
  %t15181 = load i64, i64* %t15180, align 4
  store i64 %t15181, i64* %t15178, align 4
  %t15184 = load i8*, i8** %t15210, align 8
  %t15186.not = icmp ult i8* %t15184, %stackTop.148
  br i1 %t15186.not, label %L_777, label %L_776

L_1:                                              ; preds = %L_0
  %t15193 = bitcast i8* %stackTop.0 to i64*
  store i64 84, i64* %t15193, align 4
  %t15195 = getelementptr inbounds i8, i8* %stackTop.0, i64 8
  store i8* %frontier.0, i8** %t15197, align 8
  store i8* %t15195, i8** %t15200, align 8
  tail call void @GC_collect(i8* nonnull %gcState, i64 0, i32 0)
  %t15205 = load i8*, i8** %t15197, align 8
  %t15208 = load i8*, i8** %t15200, align 8
  %t15190 = getelementptr inbounds i8, i8* %t15208, i64 -8
  br label %L_1682

L_0:                                              ; preds = %doSwitchNextBlock
  %t15211 = load i8*, i8** %t15210, align 8
  %t15213.not = icmp ult i8* %t15211, %stackTop.0
  br i1 %t15213.not, label %L_1, label %L_1682
}

declare hidden void @GC_collect(i8*, i64, i32) local_unnamed_addr

declare hidden i32 @Posix_FileSys_Stat_getGId() local_unnamed_addr

declare hidden void @Stdio_print(i8*) local_unnamed_addr

declare hidden void @GC_sequenceCopy(i8*, i8*, i64, i8*, i64, i64) local_unnamed_addr

declare hidden i64 @Posix_IO_lseek(i32, i64, i32) local_unnamed_addr

declare hidden i64 @MLton_unreachable() local_unnamed_addr

declare hidden i64 @Posix_FileSys_Stat_getINo() local_unnamed_addr

declare hidden i64 @Posix_FileSys_Stat_getSize() local_unnamed_addr

declare hidden i32 @Posix_FileSys_ST_isReg(i32) local_unnamed_addr

declare hidden i32 @Posix_FileSys_Stat_fstat(i32) local_unnamed_addr

declare hidden i64 @Posix_FileSys_Stat_getATime() local_unnamed_addr

declare hidden i8* @IntInf_toString(i8*, i8*, i32, i64) local_unnamed_addr

declare hidden i64 @Posix_FileSys_Stat_getCTime() local_unnamed_addr

declare hidden i32 @Posix_IO_close(i32) local_unnamed_addr

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

declare hidden i64 @Posix_FileSys_Stat_getDev() local_unnamed_addr

declare hidden i32 @Posix_Error_getErrno() local_unnamed_addr

declare hidden i64 @Posix_FileSys_Stat_getMTime() local_unnamed_addr

declare hidden i32 @IEEEReal_getRoundingMode() local_unnamed_addr

declare hidden i8* @IntInf_sub(i8*, i8*, i8*, i64) local_unnamed_addr

declare hidden i8* @IntInf_quot(i8*, i8*, i8*, i64) local_unnamed_addr

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare { i32, i1 } @llvm.ssub.with.overflow.i32(i32, i32) #1

declare hidden i32 @Posix_ProcEnv_isatty(i32) local_unnamed_addr

declare hidden i64 @Posix_Error_strError(i32) local_unnamed_addr

declare hidden void @MLton_heapCheckTooLarge() local_unnamed_addr

declare hidden i32 @IntInf_compare(i8*, i8*, i8*) local_unnamed_addr

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare { i32, i1 } @llvm.sadd.with.overflow.i32(i32, i32) #1

declare hidden i64 @Real64_gdtoa(double, i32, i32, i32, i8*) local_unnamed_addr

declare hidden void @MLton_halt(i8*, i32) local_unnamed_addr

declare hidden i32 @Time_getTimeOfDay(i8*, i8*) local_unnamed_addr

declare hidden i32 @Posix_FileSys_Stat_getUId() local_unnamed_addr

declare hidden i64 @Posix_IO_writeChar8Arr(i32, i8*, i32, i64) local_unnamed_addr

declare hidden i8* @IntInf_neg(i8*, i8*, i64) local_unnamed_addr

declare hidden i8* @IntInf_add(i8*, i8*, i8*, i64) local_unnamed_addr

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #1

declare hidden void @MLton_bug(i8*) local_unnamed_addr

declare hidden i64 @Posix_IO_writeChar8Vec(i32, i8*, i32, i64) local_unnamed_addr

declare hidden i8* @IntInf_mul(i8*, i8*, i8*, i64) local_unnamed_addr

declare hidden i32 @Posix_FileSys_Stat_getMode() local_unnamed_addr

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

declare hidden i8* @GC_sequenceAllocate(i8*, i64, i64, i64) local_unnamed_addr

declare hidden double @Real64_strtor(i8*, i32) local_unnamed_addr

declare hidden i64 @Posix_FileSys_Stat_getNLink() local_unnamed_addr

; Function Attrs: argmemonly nofree nounwind willreturn writeonly
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #2

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn }
attributes #1 = { nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { argmemonly nofree nounwind willreturn writeonly }

!0 = distinct !{!0, !1}
!1 = !{!"llvm.loop.isvectorized", i32 1}
!2 = !{!3}
!3 = distinct !{!3, !4}
!4 = distinct !{!4, !"LVerDomain"}
!5 = !{!6}
!6 = distinct !{!6, !4}
!7 = distinct !{!7, !1}
!8 = distinct !{!8, !1}
!9 = !{!10}
!10 = distinct !{!10, !11}
!11 = distinct !{!11, !"LVerDomain"}
!12 = !{!13}
!13 = distinct !{!13, !11}
!14 = distinct !{!14, !1}
!15 = distinct !{!15, !1}
!16 = !{!17}
!17 = distinct !{!17, !18}
!18 = distinct !{!18, !"LVerDomain"}
!19 = !{!20}
!20 = distinct !{!20, !18}
!21 = distinct !{!21, !1}
!22 = distinct !{!22, !1}
!23 = !{!24}
!24 = distinct !{!24, !25}
!25 = distinct !{!25, !"LVerDomain"}
!26 = !{!27}
!27 = distinct !{!27, !25}
!28 = distinct !{!28, !1}
!29 = distinct !{!29, !1}
!30 = !{!31}
!31 = distinct !{!31, !32}
!32 = distinct !{!32, !"LVerDomain"}
!33 = !{!34}
!34 = distinct !{!34, !32}
!35 = distinct !{!35, !1}
!36 = distinct !{!36, !1}
!37 = !{!38}
!38 = distinct !{!38, !39}
!39 = distinct !{!39, !"LVerDomain"}
!40 = !{!41}
!41 = distinct !{!41, !39}
