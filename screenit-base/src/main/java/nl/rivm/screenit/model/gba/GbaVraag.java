
package nl.rivm.screenit.model.gba;

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
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.RedenOpnieuwAanvragenClientgegevens;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.GbaVraagType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "algemeen", indexes = @Index(name = "idx_gba_vraag_bsn", columnList = "bsn") )
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class GbaVraag extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY)
	private Client client;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datum;

	private boolean verstuurd;

	private String uniqueBatchId;

	@Enumerated(EnumType.STRING)
	private GbaVraagType vraagType;

	private String bsn;

	@ManyToOne(fetch = FetchType.LAZY)
	private ScreeningOrganisatie screeningOrganisatie;

	@Enumerated(EnumType.STRING)
	private RedenOpnieuwAanvragenClientgegevens reden;

	private Boolean reactieOntvangen;

	private String aanvullendeInformatie;

	public Client getClient()
	{
		return client;
	}

	public void setClient(Client client)
	{
		this.client = client;
	}

	public Date getDatum()
	{
		return datum;
	}

	public void setDatum(Date datum)
	{
		this.datum = datum;
	}

	public boolean isVerstuurd()
	{
		return verstuurd;
	}

	public void setVerstuurd(boolean verstuurd)
	{
		this.verstuurd = verstuurd;
	}

	public GbaVraagType getVraagType()
	{
		return vraagType;
	}

	public void setVraagType(GbaVraagType vraagType)
	{
		this.vraagType = vraagType;
	}

	public String getBsn()
	{
		return bsn;
	}

	public void setBsn(String bsn)
	{
		this.bsn = bsn;
	}

	public String getUniqueBatchId()
	{
		return uniqueBatchId;
	}

	public void setUniqueBatchId(String uniqueBatchId)
	{
		this.uniqueBatchId = uniqueBatchId;
	}

	public RedenOpnieuwAanvragenClientgegevens getReden()
	{
		return reden;
	}

	public void setReden(RedenOpnieuwAanvragenClientgegevens reden)
	{
		this.reden = reden;
	}

	public ScreeningOrganisatie getScreeningOrganisatie()
	{
		return screeningOrganisatie;
	}

	public void setScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		this.screeningOrganisatie = screeningOrganisatie;
	}

	public Boolean getReactieOntvangen()
	{
		return reactieOntvangen;
	}

	public void setReactieOntvangen(Boolean reactieOntvangen)
	{
		this.reactieOntvangen = reactieOntvangen;
	}

	public String getAanvullendeInformatie()
	{
		return aanvullendeInformatie;
	}

	public void setAanvullendeInformatie(String aanvullendeInformatie)
	{
		this.aanvullendeInformatie = aanvullendeInformatie;
	}

}
