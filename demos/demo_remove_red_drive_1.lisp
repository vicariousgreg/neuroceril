
            # Construct initial environment
            (new Environment 'NIL '(
                (left_gripper gripping none)
                (right_gripper gripping none)
                (dock-case identity DockCase)
                (dock-body identity DockDrawer)
                (c4 identity Cartridge)
                (dock-body_8_2 identity DockSwitch)
                (discard-bin identity Block)
            ))

            # Construct demo (list of (action . env_updates))
            # (open_dock_drawer dock-body)
            ((move_arm_and_grasp right_gripper dock-body)
                (right_gripper gripping dock-body))
            ((move_grasped_object right_gripper dock-case true))
            ((release right_gripper)
                (right_gripper gripping none))

            # (press_dock_toggle right_gripper dock-body_8_2 2.0)
            ((press_dock_toggle right_gripper dock-body_8_2 2.0))

            # (discard_object c4)
            ((move_arm_and_grasp right_gripper c4)
                (right_gripper gripping c4))
            ((move_grasped_object right_gripper discard-bin true))
            ((release right_gripper)
                (right_gripper gripping none))

            NIL

