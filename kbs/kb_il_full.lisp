            # Construct causal knowledge base
            (var recipes (list

                # move_unobstructed_object ->
                #     move_arm_and_grasp
                (new Recipe
                    '(move_unobstructed_object
                        obj
                        gripper
                        'NIL
                        'true)
                    '(
                        ((move_arm_and_grasp gripper obj))
                    ))

                # move_unobstructed_object ->
                #     put_down_grasped_object
                (new Recipe
                    '(move_unobstructed_object
                        (try ((env gripper) 'gripping))
                        loc
                        offset)
                    '(
                        ((put_down_grasped_object gripper loc offset))
                    ))

                # move_object ->
                #     move_unobstructed_object
                (new Recipe
                    '(move_object obj loc offset)
                    '(
                        ((move_unobstructed_object obj loc offset))
                    ))

                # move_unobstructed_object_to_free_spot ->
                #     move_unobstructed_object
                # location must not be a gripper
                # location must be DockCase or discard bin
                (new Recipe
                    '(move_unobstructed_object_to_free_spot obj loc)
                    '(
                        ((move_unobstructed_object obj loc offset)
                            (and
                                (not (or
                                    (eq loc 'left_gripper)
                                    (eq loc 'right_gripper)))
                                (or
                                    (eq 'DockCase
                                        (try
                                            ((env loc) 'identity)))
                                    (eq 'discard-bin loc))))
                    ))

                # move_object_to_free_spot ->
                #     move_object_to_free_spot
                # location must be dock-case_6 or discard bin
                (new Recipe
                    '(move_object_to_free_spot obj loc)
                    '(
                        ((move_object obj loc offset)
                            (or (eq loc 'dock-case_6)
                                (eq loc 'discard-bin)))
                    ))

                # discard_object ->
                #     move_object_to_free_spot
                # location must be discard bin
                (new Recipe
                    '(discard_object obj)
                    '(
                        ((move_object_to_free_spot obj loc)
                            (eq loc 'discard-bin)
                        )
                    ))

                # put_down_grasped_object ->
                #     move_grasped_object
                #     release
                # gripped object must not be DockDrawer
                (new Recipe
                    '(put_down_grasped_object gripper loc offset)
                    '(
                        ((move_grasped_object gripper loc offset)
                            (not (eq 'DockDrawer
                                    (try
                                        ((env
                                            ((env gripper)
                                                'gripping))
                                            'identity)))))
                        ((release gripper))
                    ))

                # move_unobstructed_object ->
                #     move_arm_and_grasp
                #     put_down_grasped_object
                # object must not be DockDrawer
                (new Recipe
                    '(move_unobstructed_object obj loc offset)
                    '(
                        ((move_arm_and_grasp gripper obj)
                            (not (eq 'DockDrawer
                                (try ((env obj) 'identity)))))
                        ((put_down_grasped_object gripper loc offset))
                    ))

                # open_dock_drawer ->
                #     move_arm_and_grasp
                #     move_grasped_object
                #     release
                # object must be DockDrawer
                # offset must be true, indicating that final position is open
                (new Recipe
                    '(open_dock_drawer obj)
                    '(
                        ((move_arm_and_grasp gripper obj)
                            (eq 'DockDrawer
                                (try
                                    ((env obj) 'identity))))
                        ((move_grasped_object gripper loc offset)
                            offset)
                        ((release gripper))
                      ))

                # close_dock_drawer ->
                #     move_arm_and_grasp
                #     move_grasped_object
                #     release
                # object must be DockDrawer
                # offset must be false, indicating that final position is closed
                (new Recipe
                    '(close_dock_drawer obj)
                    '(
                        ((move_arm_and_grasp gripper obj)
                            (eq 'DockDrawer
                                (try
                                    ((env obj) 'identity))))
                        ((move_grasped_object gripper loc offset)
                            (not offset))
                        ((release gripper))
                      ))
            ))
