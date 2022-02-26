            # Construct causal knowledge base
            (var recipes (list

                # discard_object ->
                #     move_object
                # location must be discard bin
                (new Recipe
                    '(discard_object obj)
                    '(
                        ((move_object obj loc offset)
                            (eq loc 'discard-bin))
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

                # move_object ->
                #     move_arm_and_grasp
                #     put_down_grasped_object
                # object must not be DockDrawer
                (new Recipe
                    '(move_object obj loc offset)
                    '(
                        ((move_arm_and_grasp gripper obj)
                            (not (eq 'DockDrawer
                                (try
                                    ((env obj) 'identity)))))
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
