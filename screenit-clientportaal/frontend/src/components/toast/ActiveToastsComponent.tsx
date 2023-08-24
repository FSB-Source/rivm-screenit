/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {useDispatch, useSelector} from "react-redux"
import {State} from "../../datatypes/State"
import React from "react"
import styles from "./ActiveToastsComponent.module.scss"
import classNames from "classnames"
import {createHideToastAction} from "../../actions/ToastAction"
import {BevolkingsonderzoekStyle} from "../../datatypes/Bevolkingsonderzoek"
import {useSelectedBvo} from "../../utils/Hooks"
import {ToastMessageType} from "../../datatypes/toast/ToastMessage"
import ErrorToast from "./ErrorToast"
import DefaultToast from "./DefaultToast"

const ActiveToastsComponent = () => {
    const toasts = useSelector((state: State) => state.toasts)
    const dispatch = useDispatch()
    const selectedBvo = useSelectedBvo()

    return (
        <div
            className={classNames(styles.overlay, toasts.length === 0 && styles.hidden, selectedBvo && BevolkingsonderzoekStyle[selectedBvo])}>
            {toasts.map((toast, index) => {
                switch (toast.type) {
                    case ToastMessageType.ERROR:
                        return <ErrorToast key={index} title={toast.title} description={toast.description} index={index}
                                           clickClearButton={() => dispatch(createHideToastAction(index))}/>
                    default:
                        return <DefaultToast key={index} title={toast.title} description={toast.description} index={index}
                                             clickClearButton={() => dispatch(createHideToastAction(index))}/>
                }
            })}
        </div>
    )
}

export default ActiveToastsComponent
