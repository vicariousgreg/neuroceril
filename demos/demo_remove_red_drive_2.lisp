
            # Construct initial environment
            (new Environment 'NIL '(
                (left_gripper gripping none)
                (right_gripper gripping none)
                (dock-case identity DockCase)
                (dock-body identity DockDrawer)
                (c3 identity Cartridge)
                (dock-body_7_2 identity DockSwitch)
                (discard-bin identity Block)
            ))

            # Construct demo (list of (action . env_updates))
            # (open_dock_drawer dock-body)
            ((move_arm_and_grasp right_gripper dock-body)
                (right_gripper gripping dock-body))
            ((move_grasped_object right_gripper dock-case true))
            ((release right_gripper)
                (right_gripper gripping none))

            # (press_dock_toggle left_gripper dock-body_7_2 2.0)
            ((press_dock_toggle left_gripper dock-body_7_2 2.0))

            # (discard_object c3)
            ((move_arm_and_grasp left_gripper c3)
                (left_gripper gripping c3))
            ((move_grasped_object left_gripper discard-bin true))
            ((release left_gripper)
                (left_gripper gripping none))

            # (close_dock_drawer dock-body)
            ((move_arm_and_grasp right_gripper dock-body)
                (right_gripper gripping dock-body))
            ((move_grasped_object right_gripper dock-case false))
            ((release right_gripper)

                NIL
            )
