/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import classNames from "classnames"
import styles from "./TabelPagineringComponent.module.scss"
import React from "react"

export interface TabelPagineringComponentProps {
	pageCount: number;
	page: number;
	setPage: (page: number) => void;
}

const TabelPagineringComponent = (props: TabelPagineringComponentProps) => {
	return <nav className={styles.style}>
		<ul className="pagination justify-content-end">
			<li className={classNames("page-item", props.page === 1 && "disabled")}>
				<div className={"page-link"} onClick={() => props.page > 1 && props.setPage(props.page - 1)}>
					<span aria-hidden="true">&laquo;</span>
				</div>
			</li>
			{[...Array(props.pageCount)].map((x, i) =>
				(props.page < 5 ? i < 9 : props.page + 5 > props.pageCount ? i > Math.ceil(props.pageCount - 10) : Math.abs(props.page - (i + 1)) < 5) &&
				<li key={i} className={classNames("page-item", props.page === i + 1 && "active")}>
					<div className="page-link" onClick={() => props.setPage(i + 1)}>
						{i + 1}
					</div>
				</li>,
			)}
			<li className={classNames("page-item", props.page === props.pageCount && "disabled")}>
				<div className={"page-link"} onClick={() => props.page < props.pageCount && props.setPage(props.page + 1)}>
					<span aria-hidden="true">&raquo;</span>
				</div>
			</li>
		</ul>
	</nav>
}

export default TabelPagineringComponent
