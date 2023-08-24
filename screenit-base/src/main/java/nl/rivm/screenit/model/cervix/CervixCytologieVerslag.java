package nl.rivm.screenit.model.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.verslag.CervixVerslag;
import nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieVerslagContent;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.hibernate.annotations.Proxy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Proxy(lazy = true)
@Table(
	uniqueConstraints = { @UniqueConstraint(columnNames = "verslagContent") },
	indexes = {
		@Index(name = "idx_CERVIX_CYTOLOGIE_VERSLAG_CYTOLOGIE_UITSLAG", columnList = "cytologieUitslag") })
@Audited
public class CervixCytologieVerslag extends CervixVerslag<CervixCytologieVerslagContent>
{

	private static final long serialVersionUID = 1L;

	@OneToOne(mappedBy = "cytologieVerslag", optional = false, fetch = FetchType.LAZY)
	private CervixUitstrijkje uitstrijkje;

	@OneToOne(optional = false, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
	@NotAudited
	private CervixCytologieVerslagContent verslagContent;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private CervixCytologieUitslag cytologieUitslag;

	@ElementCollection
	@Temporal(TemporalType.TIMESTAMP)
	@CollectionTable(schema = "cervix", name = "cytologie_verslag_herzieningen_ontvangen")
	@NotAudited
	private List<Date> herzieningenOntvangen = new ArrayList<>();

	@NotAudited
	@Column(nullable = false)
	private String patholoogNaam;

	@Override
	public CervixCytologieVerslagContent getVerslagContent()
	{
		return verslagContent;
	}

	@Override
	public void setVerslagContent(CervixCytologieVerslagContent verslagContent)
	{
		this.verslagContent = verslagContent;
	}

	public CervixUitstrijkje getUitstrijkje()
	{
		return uitstrijkje;
	}

	public void setUitstrijkje(CervixUitstrijkje uitstrijkje)
	{
		this.uitstrijkje = uitstrijkje;
	}

	public BMHKLaboratorium getLaboratorium()
	{
		return (BMHKLaboratorium) HibernateHelper.deproxy(getUitvoerderOrganisatie());
	}

	public void setLaboratorium(BMHKLaboratorium laboratorium)
	{
		setUitvoerderOrganisatie(laboratorium);
	}

	public CervixCytologieUitslag getCytologieUitslag()
	{
		return cytologieUitslag;
	}

	public void setCytologieUitslag(CervixCytologieUitslag cytologieUitslag)
	{
		this.cytologieUitslag = cytologieUitslag;
	}

	public List<Date> getHerzieningenOntvangen()
	{
		return herzieningenOntvangen;
	}

	public void setHerzieningenOntvangen(List<Date> herzieningenOntvangen)
	{
		this.herzieningenOntvangen = herzieningenOntvangen;
	}

	public String getPatholoogNaam()
	{
		return patholoogNaam;
	}

	public void setPatholoogNaam(String patholoogNaam)
	{
		this.patholoogNaam = patholoogNaam;
	}
}
