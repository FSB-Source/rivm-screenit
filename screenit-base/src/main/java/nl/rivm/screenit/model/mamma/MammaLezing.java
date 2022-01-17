package nl.rivm.screenit.model.mamma;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaBeperktBeoordeelbaarReden;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingMbber;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingRadioloog;
import nl.rivm.screenit.model.mamma.enums.MammaLezingType;
import nl.rivm.screenit.model.mamma.enums.MammaNevenbevindingen;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.annotations.Check;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "lezing")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
@Check(
	constraints = "(lezing.lezing_type = 'DISCREPANTIE_LEZING' AND lezing.birads_links = 'GEEN' AND lezing.birads_rechts = 'GEEN') "
		+ "OR ((lezing.birads_links != 'GEEN' OR lezing.birads_rechts != 'GEEN') AND lezing.beoordelaar IS NOT NULL) " +
		"OR (lezing.birads_links = 'GEEN' AND lezing.birads_rechts = 'GEEN' AND lezing.beperkt_beoordeelbaar_reden = 'GEEN_BEOORDELING_MOGELIJK')")
public class MammaLezing extends AbstractHibernateObject
{
	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date beoordelingDatum;

	@Transient 
	private MammaBeoordeling beoordeling;

	@ManyToOne(fetch = FetchType.LAZY)
	private InstellingGebruiker beoordelaar;

	private boolean onervarenRadioloog;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaBIRADSWaarde biradsLinks;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaBIRADSWaarde biradsRechts;

	@Column(length = HibernateMagicNumber.L255)
	private String biradsOpmerking;

	@OneToMany(mappedBy = "lezing", fetch = FetchType.LAZY, orphanRemoval = true)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private List<MammaLaesie> laesies = new ArrayList<>();

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaLezingType lezingType;

	@Column(length = HibernateMagicNumber.L255)
	@Enumerated(EnumType.STRING)
	private MammaBeperktBeoordeelbaarReden beperktBeoordeelbaarReden;

	@Column(length = HibernateMagicNumber.L255)
	private String waaromGeenBeoordelingMogelijk;

	@ElementCollection(targetClass = MammaNevenbevindingen.class, fetch = FetchType.EAGER)
	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	@CollectionTable(schema = "mamma", name = "lezing_nevenbevindingen")
	private List<MammaNevenbevindingen> nevenbevindingen = new ArrayList<>();

	@Column(length = HibernateMagicNumber.L255, nullable = true)
	private String nevenbevindingOpmerking;

	@ElementCollection(targetClass = MammaLezingRedenenFotobesprekingRadioloog.class)
	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	@CollectionTable(schema = "mamma", name = "lezing_redenen_fotobespreking_radioloog")
	private List<MammaLezingRedenenFotobesprekingRadioloog> redenenFotobesprekingRadioloog = new ArrayList<>();

	@ElementCollection(targetClass = MammaLezingRedenenFotobesprekingMbber.class)
	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	@CollectionTable(schema = "mamma", name = "lezing_redenen_fotobespreking_mbber")
	private List<MammaLezingRedenenFotobesprekingMbber> redenenFotobesprekingMbber = new ArrayList<>();

	public Date getBeoordelingDatum()
	{
		return beoordelingDatum;
	}

	public void setBeoordelingDatum(Date beoordelingDatum)
	{
		this.beoordelingDatum = beoordelingDatum;
	}

	@Transient
	public MammaBeoordeling getBeoordeling()
	{
		return beoordeling;
	}

	@Transient
	public void setBeoordeling(MammaBeoordeling beoordeling)
	{
		this.beoordeling = beoordeling;
	}

	public InstellingGebruiker getBeoordelaar()
	{
		return beoordelaar;
	}

	public void setBeoordelaar(InstellingGebruiker beoordelaar)
	{
		this.beoordelaar = beoordelaar;
	}

	public boolean isOnervarenRadioloog()
	{
		return onervarenRadioloog;
	}

	public void setOnervarenRadioloog(boolean onervarenRadioloog)
	{
		this.onervarenRadioloog = onervarenRadioloog;
	}

	public MammaBIRADSWaarde getBiradsLinks()
	{
		return biradsLinks;
	}

	public void setBiradsLinks(MammaBIRADSWaarde biradsLinks)
	{
		this.biradsLinks = biradsLinks;
	}

	public MammaBIRADSWaarde getBiradsRechts()
	{
		return biradsRechts;
	}

	public void setBiradsRechts(MammaBIRADSWaarde biradsRechts)
	{
		this.biradsRechts = biradsRechts;
	}

	public String getBiradsOpmerking()
	{
		return biradsOpmerking;
	}

	public void setBiradsOpmerking(String biradsOpmerking)
	{
		this.biradsOpmerking = biradsOpmerking;
	}

	public List<MammaLaesie> getLaesies()
	{
		return laesies;
	}

	public void setLaesies(List<MammaLaesie> laesies)
	{
		this.laesies = laesies;
	}

	public MammaLezingType getLezingType()
	{
		return lezingType;
	}

	public void setLezingType(MammaLezingType lezingType)
	{
		this.lezingType = lezingType;
	}

	public MammaBeperktBeoordeelbaarReden getBeperktBeoordeelbaarReden()
	{
		return beperktBeoordeelbaarReden;
	}

	public void setBeperktBeoordeelbaarReden(MammaBeperktBeoordeelbaarReden beperktBeoordeelbaarReden)
	{
		this.beperktBeoordeelbaarReden = beperktBeoordeelbaarReden;
	}

	public String getWaaromGeenBeoordelingMogelijk()
	{
		return waaromGeenBeoordelingMogelijk;
	}

	public void setWaaromGeenBeoordelingMogelijk(String waaromGeenBeoordelingMogelijk)
	{
		this.waaromGeenBeoordelingMogelijk = waaromGeenBeoordelingMogelijk;
	}

	public List<MammaNevenbevindingen> getNevenbevindingen()
	{
		return nevenbevindingen;
	}

	public void setNevenbevindingen(List<MammaNevenbevindingen> nevenbevindingen)
	{
		this.nevenbevindingen = nevenbevindingen;
	}

	public String getNevenbevindingOpmerking()
	{
		return nevenbevindingOpmerking;
	}

	public void setNevenbevindingOpmerking(String nevenbevindingOpmerking)
	{
		this.nevenbevindingOpmerking = nevenbevindingOpmerking;
	}

	public List<MammaLezingRedenenFotobesprekingRadioloog> getRedenenFotobesprekingRadioloog()
	{
		return redenenFotobesprekingRadioloog;
	}

	public void setRedenenFotobesprekingRadioloog(List<MammaLezingRedenenFotobesprekingRadioloog> redenenFotobesprekingRadioloog)
	{
		this.redenenFotobesprekingRadioloog = redenenFotobesprekingRadioloog;
	}

	public List<MammaLezingRedenenFotobesprekingMbber> getRedenenFotobesprekingMbber()
	{
		return redenenFotobesprekingMbber;
	}

	public void setRedenenFotobesprekingMbber(List<MammaLezingRedenenFotobesprekingMbber> redenenFotobesprekingMbber)
	{
		this.redenenFotobesprekingMbber = redenenFotobesprekingMbber;
	}
}
