/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */
import React from "react"
import styles from "./ArrowIconComponent.module.scss"
import classNames from "classnames"
import {ReactComponent as ArrowDown} from "../../scss/media/ArrowDown.svg"
import {ReactComponent as ArrowRight} from "../../scss/media/ArrowRight.svg"
import {ReactComponent as ArrowUp} from "../../scss/media/ArrowUp.svg"
import {ReactComponent as ArrowLeft} from "../../scss/media/ArrowLeft.svg"
import {assertUnreachable} from "../../utils/EnumUtil"

export type ArrowIconProps = {
    className?: string,
    type: ArrowType,
}

export enum ArrowType {
    "ARROW_UP",
    "ARROW_DOWN",
    "ARROW_LEFT",
    "ARROW_RIGHT",
    "CHEVRON_UP",
    "CHEVRON_DOWN",
    "CHEVRON_LEFT",
    "CHEVRON_RIGHT",
}

const ArrowIconComponent = (props: ArrowIconProps) => {

    switch (props.type) {
        case ArrowType.ARROW_UP:
            return (
                <div className={classNames(styles.svg_icon, props.className)}>
                    <ArrowUp/>
                </div>
            )
        case ArrowType.ARROW_DOWN:
            return (
                <div className={classNames(styles.svg_icon, props.className)}>
                    <ArrowDown/>
                </div>
            )
        case ArrowType.ARROW_LEFT:
            return (
                <div className={classNames(styles.svg_icon, props.className, styles.left)}>
                    <ArrowLeft/>
                </div>
            )
        case ArrowType.ARROW_RIGHT:
            return (
                <div className={classNames(styles.svg_icon, props.className)}>
                    <ArrowRight/>
                </div>
            )
        case ArrowType.CHEVRON_UP:
            return (
                <div className={classNames(styles.icon, props.className)}>
                    <i className={classNames(props.className, "material-icons")}>chevron_up</i>
                </div>
            )
        case ArrowType.CHEVRON_DOWN:
            return (
                <div className={classNames(styles.icon, props.className)}>
                    <i className={classNames(props.className, "material-icons")}>chevron_down</i>
                </div>
            )
        case ArrowType.CHEVRON_LEFT:
            return (
                <div className={classNames(styles.icon, props.className)}>
                    <i className="material-icons">chevron_left</i>
                </div>
            )
        case ArrowType.CHEVRON_RIGHT:
            return (
                <div className={classNames(styles.icon, props.className)}>
                    <i className="material-icons">chevron_right</i>
                </div>
            )
        default:
            assertUnreachable(props.type)
    }

}

export default ArrowIconComponent
