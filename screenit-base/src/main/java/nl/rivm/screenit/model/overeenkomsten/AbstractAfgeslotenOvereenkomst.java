
package nl.rivm.screenit.model.overeenkomsten;

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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.DiscriminatorColumn;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Index;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.UploadDocument;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "gedeeld", indexes = { @Index(name = "IDX_AFG_OVEREENKOMST_AKKOORD", columnList = "akkoordDatum, eindDatum, startDatum") })
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(length = HibernateMagicNumber.L100)
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
public abstract class AbstractAfgeslotenOvereenkomst implements HibernateObject
{

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private Long id;

	@Column(unique = true)
	private String code;

	@Temporal(TemporalType.DATE)
	private Date startDatum;

	@Temporal(TemporalType.DATE)
	private Date eindDatum;

	@Temporal(TemporalType.DATE)
	private Date akkoordDatum;

	private boolean nieuwereOvereenkomst = false;

	@ManyToOne(optional = false)
	private ScreeningOrganisatie screeningOrganisatie;

	@ManyToOne
	private Overeenkomst overeenkomst;

	private int volgnummer;

	@ManyToOne
	private UploadDocument gescandDocument;

	private boolean teAccoderen;

	@Override
	public Long getId()
	{
		return id;
	}

	public void setId(Long id)
	{
		this.id = id;
	}

	public String getCode()
	{
		return code;
	}

	public void setCode(String code)
	{
		this.code = code;
	}

	public Date getStartDatum()
	{
		return startDatum;
	}

	public void setStartDatum(Date startDatum)
	{
		this.startDatum = startDatum;
	}

	public Date getEindDatum()
	{
		return eindDatum;
	}

	public void setEindDatum(Date eindDatum)
	{
		this.eindDatum = eindDatum;
	}

	public Date getAkkoordDatum()
	{
		return akkoordDatum;
	}

	public void setAkkoordDatum(Date akkoordDatum)
	{
		this.akkoordDatum = akkoordDatum;
	}

	public boolean isNieuwereOvereenkomst()
	{
		return nieuwereOvereenkomst;
	}

	public void setNieuwereOvereenkomst(boolean nieuwereOvereenkomst)
	{
		this.nieuwereOvereenkomst = nieuwereOvereenkomst;
	}

	public ScreeningOrganisatie getScreeningOrganisatie()
	{
		return screeningOrganisatie;
	}

	public void setScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		this.screeningOrganisatie = screeningOrganisatie;
	}

	public Overeenkomst getOvereenkomst()
	{
		return overeenkomst;
	}

	public void setOvereenkomst(Overeenkomst overeenkomst)
	{
		this.overeenkomst = overeenkomst;
	}

	public int getVolgnummer()
	{
		return volgnummer;
	}

	public void setVolgnummer(int volgnummer)
	{
		this.volgnummer = volgnummer;
	}

	public UploadDocument getGescandDocument()
	{
		return gescandDocument;
	}

	public void setGescandDocument(UploadDocument gescandDocument)
	{
		this.gescandDocument = gescandDocument;
	}

	public boolean isTeAccoderen()
	{
		return teAccoderen;
	}

	public void setTeAccoderen(boolean teAccoderen)
	{
		this.teAccoderen = teAccoderen;
	}

}
