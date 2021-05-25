package nl.rivm.screenit.model.project;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.vragenlijsten.VragenlijstAntwoordenHolder;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "algemeen")
public class ProjectVragenlijstAntwoordenHolder extends VragenlijstAntwoordenHolder<ProjectVragenlijst, ProjectVragenlijstAntwoordenHolder>
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	@Cascade({ CascadeType.SAVE_UPDATE })
	private ProjectVragenlijst vragenlijst;

	@Enumerated(EnumType.STRING)
	private ProjectVragenlijstStatus status;

	@Temporal(TemporalType.TIMESTAMP)
	private Date laatstGewijzigd;

	@OneToOne(fetch = FetchType.LAZY)
	private ScannedVragenlijst scannedVragenlijst;

	@OneToOne(mappedBy = "vragenlijstAntwoordenHolder")
	private ProjectBrief projectBrief;

	@Override
	public ProjectVragenlijst getVragenlijst()
	{
		return vragenlijst;
	}

	@Override
	public void setVragenlijst(ProjectVragenlijst vragenlijst)
	{
		this.vragenlijst = vragenlijst;
	}

	public ProjectVragenlijstStatus getStatus()
	{
		return status;
	}

	public void setStatus(ProjectVragenlijstStatus status)
	{
		this.status = status;
	}

	public Date getLaatstGewijzigd()
	{
		return laatstGewijzigd;
	}

	public void setLaatstGewijzigd(Date laatstGewijzigd)
	{
		this.laatstGewijzigd = laatstGewijzigd;
	}

	public ScannedVragenlijst getScannedVragenlijst()
	{
		return scannedVragenlijst;
	}

	public void setScannedVragenlijst(ScannedVragenlijst scannedVragenlijst)
	{
		this.scannedVragenlijst = scannedVragenlijst;
	}

	public ProjectBrief getProjectBrief()
	{
		return projectBrief;
	}
}
