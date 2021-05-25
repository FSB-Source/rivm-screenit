package nl.rivm.screenit.model.mamma;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.mamma.berichten.MammaHuisartsBericht;
import nl.rivm.screenit.model.mamma.berichten.xds.XdsStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "mamma",
	name = "beoordeling",
	uniqueConstraints = {
		@UniqueConstraint(columnNames = "eerste_lezing"), @UniqueConstraint(columnNames = "tweede_lezing"),
		@UniqueConstraint(columnNames = "discrepantie_lezing"), @UniqueConstraint(columnNames = "arbitrage_lezing"), @UniqueConstraint(columnNames = "verslag_lezing") },
	indexes = { @Index(name = "idx_mamma_beoordeling_status", columnList = "status") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
public class MammaBeoordeling extends AbstractHibernateObject
{
	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaLezing eersteLezing;

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaLezing tweedeLezing;

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaLezing discrepantieLezing;

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaLezing arbitrageLezing;

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaLezing verslagLezing;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaBeoordelingStatus status = MammaBeoordelingStatus.EERSTE_LEZING;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private InstellingGebruiker reserveringhouder;

	@Temporal(TemporalType.TIMESTAMP)
	private Date reserveringsmoment;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaOnderzoek onderzoek;

	@Column(nullable = true)
	private String afkeurreden;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private BeoordelingsEenheid beoordelingsEenheid;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaBeoordelingOpschortenReden opschortReden = MammaBeoordelingOpschortenReden.NIET_OPSCHORTEN;

	@Column(nullable = true)
	private String opschortRedenTekst;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private InstellingGebruiker opschortGebruiker;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private InstellingGebruiker toegewezenGebruiker;

	@Column(nullable = true)
	@Temporal(TemporalType.TIMESTAMP)
	private Date toegewezenOp;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "beoordeling")
	private List<MammaHuisartsBericht> huisartsBerichten = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	private UploadDocument verslagPdf;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private XdsStatus xdsVerslagStatus;

	@Column
	private String redenAnnuleren;

	public List<MammaHuisartsBericht> getHuisartsBerichten()
	{
		return huisartsBerichten;
	}

	public void setHuisartsBerichten(List<MammaHuisartsBericht> huisartsBerichten)
	{
		this.huisartsBerichten = huisartsBerichten;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date statusDatum)
	{
		this.statusDatum = statusDatum;
	}

	public MammaLezing getEersteLezing()
	{
		return eersteLezing;
	}

	public void setEersteLezing(MammaLezing eersteLezing)
	{
		this.eersteLezing = eersteLezing;
	}

	public MammaLezing getTweedeLezing()
	{
		return tweedeLezing;
	}

	public void setTweedeLezing(MammaLezing tweedeLezing)
	{
		this.tweedeLezing = tweedeLezing;
	}

	public MammaLezing getDiscrepantieLezing()
	{
		return discrepantieLezing;
	}

	public void setDiscrepantieLezing(MammaLezing discrepantie)
	{
		this.discrepantieLezing = discrepantie;
	}

	public MammaLezing getArbitrageLezing()
	{
		return arbitrageLezing;
	}

	public void setArbitrageLezing(MammaLezing consensus)
	{
		this.arbitrageLezing = consensus;
	}

	public MammaBeoordelingStatus getStatus()
	{
		return status;
	}

	public void setStatus(MammaBeoordelingStatus status)
	{
		this.status = status;
	}

	public InstellingGebruiker getReserveringhouder()
	{
		return reserveringhouder;
	}

	public void setReserveringhouder(InstellingGebruiker reserveringhouder)
	{
		this.reserveringhouder = reserveringhouder;
	}

	public Date getReserveringsmoment()
	{
		return reserveringsmoment;
	}

	public void setReserveringsmoment(Date reserveringsmoment)
	{
		this.reserveringsmoment = reserveringsmoment;
	}

	public MammaOnderzoek getOnderzoek()
	{
		return onderzoek;
	}

	public void setOnderzoek(MammaOnderzoek onderzoek)
	{
		this.onderzoek = onderzoek;
	}

	public MammaLezing getVerslagLezing()
	{
		return verslagLezing;
	}

	public void setVerslagLezing(MammaLezing verslagLezing)
	{
		this.verslagLezing = verslagLezing;
	}

	public void setAfkeurreden(String afkeurreden)
	{
		this.afkeurreden = afkeurreden;
	}

	public String getAfkeurreden()
	{
		return afkeurreden;
	}

	public BeoordelingsEenheid getBeoordelingsEenheid()
	{
		return beoordelingsEenheid;
	}

	public void setBeoordelingsEenheid(BeoordelingsEenheid beoordelingsEenheid)
	{
		this.beoordelingsEenheid = beoordelingsEenheid;
	}

	public MammaBeoordelingOpschortenReden getOpschortReden()
	{
		return opschortReden;
	}

	public void setOpschortReden(MammaBeoordelingOpschortenReden opschortenReden)
	{
		this.opschortReden = opschortenReden;
	}

	public String getOpschortRedenTekst()
	{
		return opschortRedenTekst;
	}

	public void setOpschortRedenTekst(String opschortenRedenTekst)
	{
		this.opschortRedenTekst = opschortenRedenTekst;
	}

	public InstellingGebruiker getOpschortGebruiker()
	{
		return opschortGebruiker;
	}

	public void setOpschortGebruiker(InstellingGebruiker opschortGebruiker)
	{
		this.opschortGebruiker = opschortGebruiker;
	}

	public InstellingGebruiker getToegewezenGebruiker()
	{
		return toegewezenGebruiker;
	}

	public void setToegewezenGebruiker(InstellingGebruiker toegewezenGebruiker)
	{
		this.toegewezenGebruiker = toegewezenGebruiker;
	}

	public Date getToegewezenOp()
	{
		return toegewezenOp;
	}

	public void setToegewezenOp(Date toegewezenOp)
	{
		this.toegewezenOp = toegewezenOp;
	}

	public void setVerslagPdf(UploadDocument verslag)
	{
		this.verslagPdf = verslag;
	}

	public UploadDocument getVerslagPdf()
	{
		return verslagPdf;
	}

	public XdsStatus getXdsVerslagStatus()
	{
		return xdsVerslagStatus;
	}

	public void setXdsVerslagStatus(XdsStatus xdsVerslagStatus)
	{
		this.xdsVerslagStatus = xdsVerslagStatus;
	}

	public String getRedenAnnuleren()
	{
		return redenAnnuleren;
	}

	public void setRedenAnnuleren(String redenAnnuleren)
	{
		this.redenAnnuleren = redenAnnuleren;
	}
}
