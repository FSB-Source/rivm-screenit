package nl.rivm.screenit.model.project;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.ScreeningRonde;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(
	schema = "gedeeld",
	indexes = { @Index(name = "idx_project_brief_gegenereerd", columnList = "gegenereerd"),
		@Index(name = "idx_project_brief_vervangendeprojectbrief", columnList = "vervangendeprojectbrief"),
		@Index(name = "idx_project_brief_brief", columnList = "brief")
	})
@Audited
@Getter
@Setter
public class ProjectBrief extends ClientBrief<ScreeningRonde, Afmelding, ProjectBrief>
{
	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@Cascade({ CascadeType.SAVE_UPDATE })
	@NotAudited
	private ProjectClient projectClient;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@NotAudited
	private ProjectBriefActie definitie;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	@Cascade({ CascadeType.SAVE_UPDATE })
	private ClientBrief brief;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	@Cascade({ CascadeType.SAVE_UPDATE })
	private ProjectBrief teHerinnerenBrief;

	@ManyToOne(fetch = FetchType.LAZY)
	private ProjectMergedBrieven mergedBrieven;

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade({ CascadeType.SAVE_UPDATE })
	@NotAudited
	private ProjectVragenlijstAntwoordenHolder vragenlijstAntwoordenHolder;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private ScreeningRonde screeningRonde;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private Afmelding afmelding;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private ProjectBrief herdruk;

	@Override
	public void setMergedBrieven(MergedBrieven mergedBrieven)
	{
		this.mergedBrieven = (ProjectMergedBrieven) mergedBrieven;
	}

	@Override
	public ScreeningRonde getScreeningRonde()
	{
		return screeningRonde;
	}

	@Override
	public void setScreeningRonde(ScreeningRonde screeningRonde)
	{
		this.screeningRonde = screeningRonde;
	}

	@Override
	public Afmelding getAfmelding()
	{
		return afmelding;
	}

	@Override
	public void setAfmelding(Afmelding afmelding)
	{
		this.afmelding = afmelding;
	}

}
