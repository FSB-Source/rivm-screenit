package nl.rivm.screenit.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigDecimal;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpTumorGrootteClassificatie;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Audited
@Table(schema = "mamma", name = "follow_up_radiologieverslag")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
public class MammaFollowUpRadiologieVerslag extends AbstractHibernateObject
{
	@ManyToOne(optional = false, fetch = FetchType.EAGER)
	private MammaScreeningRonde screeningRonde;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date aangemaaktOp;

	@ManyToOne(optional = false, fetch = FetchType.EAGER)
	private Instelling aangemaaktIn;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date ingevoerdOp;

	@ManyToOne(optional = true, fetch = FetchType.EAGER)
	private InstellingGebruiker ingevoerdDoor;

	@Column(nullable = true, precision = HibernateMagicNumber.P4, scale = HibernateMagicNumber.S2)
	private BigDecimal radioloogTumorGrootte;

	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	private MammaFollowUpTumorGrootteClassificatie radioloogTumorGrootteClassificatie;

	@Column(nullable = true)
	private Boolean pathologieUitgevoerd;

	@Column(nullable = false)
	private Boolean informatieBeschikbaar;

	@Column(length = 1024)
	private String conclusieEersteUitslagRadiologie;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date laatstGebeldOverPaVerslag;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date paVerslagNietTeVerwachten;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private MammaFollowUpBIRADSWaarde conclusieBirads;

	public MammaScreeningRonde getScreeningRonde()
	{
		return screeningRonde;
	}

	public void setScreeningRonde(MammaScreeningRonde screeningRonde)
	{
		this.screeningRonde = screeningRonde;
	}

	public Date getAangemaaktOp()
	{
		return aangemaaktOp;
	}

	public void setAangemaaktOp(Date aangemaaktOp)
	{
		this.aangemaaktOp = aangemaaktOp;
	}

	public Instelling getAangemaaktIn()
	{
		return aangemaaktIn;
	}

	public void setAangemaaktIn(Instelling aangemaaktIn)
	{
		this.aangemaaktIn = aangemaaktIn;
	}

	public Date getIngevoerdOp()
	{
		return ingevoerdOp;
	}

	public void setIngevoerdOp(Date ingevoerdOp)
	{
		this.ingevoerdOp = ingevoerdOp;
	}

	public InstellingGebruiker getIngevoerdDoor()
	{
		return ingevoerdDoor;
	}

	public void setIngevoerdDoor(InstellingGebruiker ingevoerdDoor)
	{
		this.ingevoerdDoor = ingevoerdDoor;
	}

	public BigDecimal getRadioloogTumorGrootte()
	{
		return radioloogTumorGrootte;
	}

	public void setRadioloogTumorGrootte(BigDecimal radioloogTumorGrootte)
	{
		this.radioloogTumorGrootte = radioloogTumorGrootte;
	}

	public MammaFollowUpTumorGrootteClassificatie getRadioloogTumorGrootteClassificatie()
	{
		return radioloogTumorGrootteClassificatie;
	}

	public void setRadioloogTumorGrootteClassificatie(MammaFollowUpTumorGrootteClassificatie radioloogTumorGrootteClassificatie)
	{
		this.radioloogTumorGrootteClassificatie = radioloogTumorGrootteClassificatie;
	}

	public Boolean getPathologieUitgevoerd()
	{
		return pathologieUitgevoerd;
	}

	public void setPathologieUitgevoerd(Boolean pathologieUitgevoerd)
	{
		this.pathologieUitgevoerd = pathologieUitgevoerd;
	}

	public Boolean getInformatieBeschikbaar()
	{
		return informatieBeschikbaar;
	}

	public void setInformatieBeschikbaar(Boolean informatieBeschikbaar)
	{
		this.informatieBeschikbaar = informatieBeschikbaar;
	}

	public String getConclusieEersteUitslagRadiologie()
	{
		return conclusieEersteUitslagRadiologie;
	}

	public void setConclusieEersteUitslagRadiologie(String conclusieEersteUitslagRadiologie)
	{
		this.conclusieEersteUitslagRadiologie = conclusieEersteUitslagRadiologie;
	}

	public Date getLaatstGebeldOverPaVerslag()
	{
		return laatstGebeldOverPaVerslag;
	}

	public void setLaatstGebeldOverPaVerslag(Date laatstGebeldOverPaVerslag)
	{
		this.laatstGebeldOverPaVerslag = laatstGebeldOverPaVerslag;
	}

	public Date getPaVerslagNietTeVerwachten()
	{
		return paVerslagNietTeVerwachten;
	}

	public void setPaVerslagNietTeVerwachten(Date paVerslagNietTeVerwachten)
	{
		this.paVerslagNietTeVerwachten = paVerslagNietTeVerwachten;
	}

	public MammaFollowUpBIRADSWaarde getConclusieBirads()
	{
		return conclusieBirads;
	}

	public void setConclusieBirads(MammaFollowUpBIRADSWaarde conclusieBirads)
	{
		this.conclusieBirads = conclusieBirads;
	}
}
