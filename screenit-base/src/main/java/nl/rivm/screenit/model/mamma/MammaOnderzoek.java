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
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.ExtraFotosReden;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekRedenFotobespreking;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.OnderbrokenOnderzoekOption;
import nl.rivm.screenit.model.mamma.enums.OnvolledigOnderzoekOption;
import nl.rivm.screenit.model.mamma.enums.SuboptimaleInsteltechniek;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.annotations.FetchMode;
import org.hibernate.annotations.FetchProfile;
import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "mamma",
	name = "onderzoek",
	uniqueConstraints = { @UniqueConstraint(columnNames = "mammografie"), @UniqueConstraint(columnNames = "signaleren"), @UniqueConstraint(columnNames = "laatste_beoordeling") },
		indexes = {
		@Index(name = "idx_mamma_onderzoek_status", columnList = "status"),
})
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
@FetchProfile(
	name = "kansberekening",
	fetchOverrides = {
		@FetchProfile.FetchOverride(entity = MammaOnderzoek.class, association = "laatsteBeoordeling", mode = FetchMode.JOIN),
		@FetchProfile.FetchOverride(entity = MammaOnderzoek.class, association = "mammografie", mode = FetchMode.JOIN),
	})
public class MammaOnderzoek extends AbstractHibernateObject
{
	@OneToOne(optional = false, fetch = FetchType.LAZY, mappedBy = "onderzoek")
	private MammaAfspraak afspraak;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date creatieDatum;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaScreeningsEenheid screeningsEenheid;

	@OneToMany(mappedBy = "onderzoek", fetch = FetchType.LAZY)
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private List<MammaBeoordeling> beoordelingen = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY)
	@Cascade(CascadeType.ALL)
	private MammaBeoordeling laatsteBeoordeling;

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaMammografie mammografie;

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaSignaleren signaleren;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaOnderzoekStatus status;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private ZorgInstelling eerderMammogramZorginstelling;

	@Column
	private Integer eerderMammogramJaartal;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private SuboptimaleInsteltechniek suboptimaleInsteltechniek;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private MammaOnderzoekRedenFotobespreking redenFotobespreking;

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	private String opmerkingMbber;

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	private String opmerkingVoorRadioloog;

	@Column(nullable = false)
	private Boolean operatieRechts;

	@Column(nullable = false)
	private Boolean operatieLinks;

	@Column
	@Enumerated(EnumType.STRING)
	private MammaAmputatie amputatie;

	@Column
	private String aanvullendeInformatieOperatie;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private OnvolledigOnderzoekOption onvolledigOnderzoek;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private OnderbrokenOnderzoekOption onderbrokenOnderzoek;

	@ElementCollection(targetClass = ExtraFotosReden.class)
	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	@CollectionTable(schema = "mamma", name = "extra_fotos_reden")
	private List<ExtraFotosReden> extraFotosRedenen = new ArrayList<>();

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	private String adviesHuisarts;

	@Column(nullable = true)
	@Temporal(TemporalType.TIMESTAMP)
	private Date afgerondOp;

	@ManyToOne(fetch = FetchType.LAZY)
	private InstellingGebruiker extraMedewerker;

	@Column(nullable = false)
	private boolean isDoorgevoerd;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "onderzoek")
	private MammaAdhocMeekijkverzoek meekijkverzoek;

	public MammaMammografie getMammografie()
	{
		return mammografie;
	}

	public void setMammografie(MammaMammografie mammografie)
	{
		this.mammografie = mammografie;
	}

	public MammaSignaleren getSignaleren()
	{
		return signaleren;
	}

	public void setSignaleren(MammaSignaleren signaleren)
	{
		this.signaleren = signaleren;
	}

	public MammaAfspraak getAfspraak()
	{
		return afspraak;
	}

	public void setAfspraak(MammaAfspraak afspraak)
	{
		this.afspraak = afspraak;
	}

	public Date getCreatieDatum()
	{
		return creatieDatum;
	}

	public void setCreatieDatum(Date creatieDatum)
	{
		this.creatieDatum = creatieDatum;
	}

	public List<MammaBeoordeling> getBeoordelingen()
	{
		return beoordelingen;
	}

	public void setBeoordelingen(List<MammaBeoordeling> beoordelingen)
	{
		this.beoordelingen = beoordelingen;
	}

	public MammaBeoordeling getLaatsteBeoordeling()
	{
		return laatsteBeoordeling;
	}

	public void setLaatsteBeoordeling(MammaBeoordeling laatsteBeoordeling)
	{
		this.laatsteBeoordeling = laatsteBeoordeling;
	}

	public MammaScreeningsEenheid getScreeningsEenheid()
	{
		return screeningsEenheid;
	}

	public void setScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		this.screeningsEenheid = screeningsEenheid;
	}

	public MammaOnderzoekStatus getStatus()
	{
		return status;
	}

	public void setStatus(MammaOnderzoekStatus status)
	{
		this.status = status;
	}

	public Integer getEerderMammogramJaartal()
	{
		return eerderMammogramJaartal;
	}

	public void setEerderMammogramJaartal(Integer eerderMammogramJaartal)
	{
		this.eerderMammogramJaartal = eerderMammogramJaartal;
	}

	public SuboptimaleInsteltechniek getSuboptimaleInsteltechniek()
	{
		return suboptimaleInsteltechniek;
	}

	public void setSuboptimaleInsteltechniek(SuboptimaleInsteltechniek suboptimaleInsteltechniek)
	{
		this.suboptimaleInsteltechniek = suboptimaleInsteltechniek;
	}

	public MammaOnderzoekRedenFotobespreking getRedenFotobespreking()
	{
		return redenFotobespreking;
	}

	public void setRedenFotobespreking(MammaOnderzoekRedenFotobespreking redenFotobespreking)
	{
		this.redenFotobespreking = redenFotobespreking;
	}

	public String getOpmerkingMbber()
	{
		return opmerkingMbber;
	}

	public void setOpmerkingMbber(String opmerkingMbber)
	{
		this.opmerkingMbber = opmerkingMbber;
	}

	public String getOpmerkingVoorRadioloog()
	{
		return opmerkingVoorRadioloog;
	}

	public void setOpmerkingVoorRadioloog(String opmerkingVoorRadioloog)
	{
		this.opmerkingVoorRadioloog = opmerkingVoorRadioloog;
	}

	public Boolean getOperatieRechts()
	{
		return operatieRechts;
	}

	public void setOperatieRechts(Boolean operatieRechts)
	{
		this.operatieRechts = operatieRechts;
	}

	public Boolean getOperatieLinks()
	{
		return operatieLinks;
	}

	public void setOperatieLinks(Boolean operatieLinks)
	{
		this.operatieLinks = operatieLinks;
	}

	public String getAanvullendeInformatieOperatie()
	{
		return aanvullendeInformatieOperatie;
	}

	public void setAanvullendeInformatieOperatie(String aanvullendeInformatieOperatie)
	{
		this.aanvullendeInformatieOperatie = aanvullendeInformatieOperatie;
	}

	public OnvolledigOnderzoekOption getOnvolledigOnderzoek()
	{
		return onvolledigOnderzoek;
	}

	public void setOnvolledigOnderzoek(OnvolledigOnderzoekOption onvolledigOnderzoek)
	{
		this.onvolledigOnderzoek = onvolledigOnderzoek;
	}

	public OnderbrokenOnderzoekOption getOnderbrokenOnderzoek()
	{
		return onderbrokenOnderzoek;
	}

	public void setOnderbrokenOnderzoek(OnderbrokenOnderzoekOption onderbrokenOnderzoek)
	{
		this.onderbrokenOnderzoek = onderbrokenOnderzoek;
	}

	public List<ExtraFotosReden> getExtraFotosRedenen()
	{
		return extraFotosRedenen;
	}

	public void setExtraFotosRedenen(List<ExtraFotosReden> extraFotosReden)
	{
		this.extraFotosRedenen = extraFotosReden;
	}

	public String getAdviesHuisarts()
	{
		return adviesHuisarts;
	}

	public void setAdviesHuisarts(String adviesHuisarts)
	{
		this.adviesHuisarts = adviesHuisarts;
	}

	public Date getAfgerondOp()
	{
		return afgerondOp;
	}

	public void setAfgerondOp(Date afgerondOp)
	{
		this.afgerondOp = afgerondOp;
	}

	public ZorgInstelling getEerderMammogramZorginstelling()
	{
		return eerderMammogramZorginstelling;
	}

	public void setEerderMammogramZorginstelling(ZorgInstelling eerderMammogramZorginstelling)
	{
		this.eerderMammogramZorginstelling = eerderMammogramZorginstelling;
	}

	public InstellingGebruiker getExtraMedewerker()
	{
		return extraMedewerker;
	}

	public void setExtraMedewerker(InstellingGebruiker extraMedewerker)
	{
		this.extraMedewerker = extraMedewerker;
	}

	public boolean isDoorgevoerd()
	{
		return isDoorgevoerd;
	}

	public void setDoorgevoerd(boolean doorgevoerd)
	{
		isDoorgevoerd = doorgevoerd;
	}

	public MammaAmputatie getAmputatie()
	{
		return amputatie;
	}

	public void setAmputatie(MammaAmputatie amputatie)
	{
		this.amputatie = amputatie;
	}

	public MammaAdhocMeekijkverzoek getMeekijkverzoek()
	{
		return meekijkverzoek;
	}

	public void setMeekijkverzoek(MammaAdhocMeekijkverzoek meekijkverzoek)
	{
		this.meekijkverzoek = meekijkverzoek;
	}
}
