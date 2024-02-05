/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import classNames from "classnames"
import styles from "./ToastComponent.module.scss"
import {Toast, ToastType} from "../../../state/datatypes/Toast"
import {useDispatch} from "react-redux"
import React, {useCallback, useEffect, useState} from "react"
import {createActionHideToast} from "../../../state/ToastsState"

export interface ToastComponentProps {
	toast: Toast;
}

const ToastComponent = (props: ToastComponentProps) => {
	const dispatch = useDispatch()
	const [isDismissed, setIsDismissed] = useState<boolean>(false)

	const dismiss = useCallback(() => {
		if (!isDismissed) {
			setIsDismissed(true)
			setTimeout(() => {
				dispatch(createActionHideToast(props.toast.id))
			}, 1000)
		}
	}, [dispatch, isDismissed, setIsDismissed, props.toast.id])

	useEffect(() => {
		setTimeout(() => {
			dismiss()
		}, 5000)
	}, [dispatch, isDismissed, dismiss])

	return <div className={classNames(styles.style, getToastStyle(props.toast.type), isDismissed && styles.dismiss, "p-4", "rounded-3", "fw-bold", "my-3")} onClick={dismiss}>
		{props.toast.message}
	</div>
}

function getToastStyle(type: ToastType) {
	switch (type) {
		case ToastType.SUCCESS:
			return styles.success
		case ToastType.INFO:
			return styles.info
		case ToastType.WARN:
			return styles.warn
		case ToastType.ERROR:
			return styles.error
	}
}

export default ToastComponent
