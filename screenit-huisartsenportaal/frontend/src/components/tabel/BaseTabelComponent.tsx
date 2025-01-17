/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import React, {useCallback, useEffect, useState} from "react"
import ScreenitBackend from "../../util/Backend"
import {AxiosResponse} from "axios"
import {PaginationDto, SortOptionsDto, SortOrder} from "../../state/datatypes/dto/TabelRequestDto"
import {useAppThunkDispatch} from "../../index"
import {loadingThunkAction} from "../../api/LoadingThunkAction"

export interface ZoekObjectToevoeging {
	property: string;
	value: object;
}

export interface ChildProps<T> {
	results?: T,
	setSortProperty: (property: string) => void;
	setZoekObjectToevoeging: (toevoeging: ZoekObjectToevoeging) => void;
	refresh: () => void;
	export: () => void;
	page: number;
	setPage: (page: number) => void;
}

export interface TabelComponentProps<T> {
	resultsPerPage: number;
	url: string;
	exportUrl?: string;
	exportFileName?: string;
	children: | ((props: ChildProps<T>) => React.ReactNode) | React.ReactNode;
}

function BaseTabelComponent<T>(props: TabelComponentProps<T>) {
	const dispatch = useAppThunkDispatch()

	const [results, setResults] = useState<T | undefined>(undefined)
	const [sortOptions, setSortOptions] = useState<SortOptionsDto | undefined>(undefined)
	const [zoekObjectToevoeging, setZoekObjectToevoeging] = useState<ZoekObjectToevoeging | undefined>(undefined)
	const [page, setPage] = useState<number>(1)

	const exportCsv = useCallback(() => {
		(props.exportUrl && props.exportFileName) && ScreenitBackend.request({
			url: props.exportUrl,
			method: "POST",
			responseType: "arraybuffer",
			reponseEncoding: "binary",
			data: {
				resultOptions: {
					first: 0,
					count: -1,
					sortOptions: sortOptions,
				} as PaginationDto, ...zoekObjectToevoeging,
				...(zoekObjectToevoeging ? {[zoekObjectToevoeging?.property]: zoekObjectToevoeging.value} : {}),
			},
			headers: {
				"Content-Type": "application/json;charset=UTF-8",
				Accept: "application/octet-stream;charset=UTF-8",
			},
		}).then((response => {
			const downloadUrl = URL.createObjectURL(new Blob([new Uint8Array([0xEF, 0xBB, 0xBF]), response.data], {type: "application/csv;charset=UTF-8"}))
			const downloadElement = document.createElement("a")
			if (typeof downloadElement.download === "undefined") {
				window.location.href = downloadUrl
			} else {
				downloadElement.href = downloadUrl
				downloadElement.download = props.exportFileName!
				document.body.appendChild(downloadElement)
				downloadElement.click()
			}
		}))
	}, [sortOptions, zoekObjectToevoeging, props.exportUrl, props.exportFileName])

	const fetchResults = useCallback(() => {
		dispatch(loadingThunkAction(async () => {
			return await ScreenitBackend.post(props.url, {
				resultOptions: {
					first: page === 1 ? 0 : (page - 1) * props.resultsPerPage,
					count: props.resultsPerPage,
					sortOptions: sortOptions,
				} as PaginationDto, ...zoekObjectToevoeging,
				...(zoekObjectToevoeging ? {[zoekObjectToevoeging?.property]: zoekObjectToevoeging.value} : {}),
			})
		})).then((response: AxiosResponse<T>) => {
			setResults(response.data)
		})
	}, [page, sortOptions, zoekObjectToevoeging, setResults, dispatch, props.resultsPerPage, props.url])

	useEffect(() => {
		fetchResults()
	}, [fetchResults, sortOptions, page])

	const childProps: ChildProps<T> = {
		results: results,
		refresh: fetchResults,
		export: exportCsv,
		page: page,
		setPage: (page: number) => {
			setPage(page)
		},
		setSortProperty: (property: string) => {
			if (sortOptions && !!sortOptions[property]) {
				setSortOptions({
					[property]: sortOptions[property] === SortOrder.ASC ? SortOrder.DESC : SortOrder.ASC,
				})
			} else {
				setSortOptions({
					[property]: SortOrder.DESC,
				})
			}
		},
		setZoekObjectToevoeging: (toevoeging: ZoekObjectToevoeging) => setZoekObjectToevoeging(toevoeging),
	}

	return React.createElement(props.children as any, childProps)
}

export default BaseTabelComponent
