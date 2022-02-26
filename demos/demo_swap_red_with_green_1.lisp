
            # Construct initial environment
            (new Environment 'NIL '(
                (left_gripper gripping none)
                (right_gripper gripping none)
                (dock-case identity DockCase)
                (dock-body identity DockDrawer)
                (dock-body_5_1 identity DockSlot)
                (c1 identity Cartridge)
                (dock-body_5_2 identity DockSwitch)
                (dock-body_7_1 identity DockSlot)
                (c3 identity Cartridge)
                (dock-body_7_2 identity DockSwitch)
            ))

            # Construct demo (list of (action . env_updates))
            # (open_dock_drawer dock-body)
            ((move_arm_and_grasp right_gripper dock-body)
                (right_gripper gripping dock-body))
            ((move_grasped_object right_gripper dock-case true))
            ((release right_gripper)
                (right_gripper gripping none))

            # (press_dock_toggle right_gripper dock-body_5_2 2.0)
            ((press_dock_toggle right_gripper dock-body_5_2 2.0))

            # (move_object c1 dock-body_7_1 false)
            ((move_arm_and_grasp right_gripper c1)
                (right_gripper gripping c1))
            ((move_grasped_object right_gripper dock-body_7_1 false))
            ((release right_gripper)
                (right_gripper gripping none))

            # (press_dock_toggle left_gripper dock-body_7_2 2.0)
            ((press_dock_toggle left_gripper dock-body_7_2 2.0))

            # (move_object c3 dock-body_5_1 false)
            ((move_arm_and_grasp left_gripper c3)
                (left_gripper gripping c3))
            ((move_grasped_object left_gripper dock-body_5_1 false))
            ((release left_gripper)
                (left_gripper gripping none))

            # (press_dock_toggle right_gripper dock-body_7_2 1.0)
            ((press_dock_toggle right_gripper dock-body_7_2 1.0))

            # (press_dock_toggle right_gripper dock-body_5_2 1.0)
            ((press_dock_toggle right_gripper dock-body_5_2 1.0))

            # (close_dock_drawer dock-body)
            ((move_arm_and_grasp right_gripper dock-body)
                (right_gripper gripping dock-body))
            ((move_grasped_object right_gripper dock-case false))
            ((release right_gripper)
                NIL
            )
