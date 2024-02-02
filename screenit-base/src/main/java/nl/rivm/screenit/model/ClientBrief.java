package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.project.ProjectBrief;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Audited
@Getter
@Setter
public abstract class ClientBrief<SR extends ScreeningRonde, AF extends Afmelding, CB extends ClientBrief> extends Brief
{
	@OneToOne(optional = true, fetch = FetchType.LAZY, mappedBy = "brief", cascade = { javax.persistence.CascadeType.PERSIST, javax.persistence.CascadeType.MERGE,
		javax.persistence.CascadeType.REMOVE })
	@Cascade({ CascadeType.SAVE_UPDATE, CascadeType.DELETE })
	private ProjectBrief projectBrief;

	private boolean vervangendeProjectBrief = false;

	@ManyToOne(optional = false, fetch = FetchType.LAZY, cascade = { javax.persistence.CascadeType.PERSIST, javax.persistence.CascadeType.MERGE })
	@Cascade({ CascadeType.SAVE_UPDATE })
	private Client client;

	public abstract AF getAfmelding();

	public abstract void setAfmelding(AF afmelding);

	public abstract SR getScreeningRonde();

	public abstract void setScreeningRonde(SR screeningRonde);

	public abstract CB getHerdruk();

	public abstract void setHerdruk(CB herdruk);
}
