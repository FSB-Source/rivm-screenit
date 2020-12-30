
package nl.rivm.screenit.main.web.gebruiker.clienten.agenda;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.mamma.MammaAfspraakService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public abstract class MammaAfspraakPanel extends GenericPanel<MammaAfspraak>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private BaseBriefService baseBriefService;

	@SpringBean
	private MammaAfspraakService afspraakService;

	public MammaAfspraakPanel(String id, IModel<Client> model)
	{
		super(id);

		Client client = model.getObject();
		hibernateService.reload(client);

		MammaDossier dossier = client.getMammaDossier();
		MammaAfspraak afspraak = null;
		if (dossier != null)
		{
			MammaScreeningRonde screeningRonde = dossier.getLaatsteScreeningRonde();
			if (screeningRonde != null && screeningRonde.getLaatsteUitnodiging() != null)
			{
				afspraak = screeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak();
			}
		}

		WebMarkupContainer inhoud = new WebMarkupContainer("inhoud");
		inhoud.setOutputMarkupId(true);
		add(inhoud);

		boolean heeftActiveAfspraak = afspraak != null && afspraak.getStatus() == MammaAfspraakStatus.GEPLAND && afspraak.getVanaf().compareTo(dateSupplier.getDate()) >= 0;

		setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_MAMMA_AFSPRAKEN, Actie.INZIEN)
			&& (Geslacht.VROUW.equals(client.getPersoon().getGeslacht()) || heeftActiveAfspraak));
		inhoud.setVisible(heeftActiveAfspraak);

		if (heeftActiveAfspraak)
		{
			setModel(ModelUtil.csModel(afspraak));
			inhoud.add(DateLabel.forDatePattern("vanaf", "EEEE dd-MM-yyyy HH:mm"));

			MammaStandplaats standplaats = afspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats();
			inhoud.add(new StandplaatsFragment("standplaats", afspraak.getVanaf(), standplaats));

			AjaxLink<MammaAfspraak> verzetten = new IndicatingAjaxLink<MammaAfspraak>("verzetten", getModel())
			{
				@Override
				public void onClick(AjaxRequestTarget target)
				{
					verzetten(target, getModelObject());
				}

			};
			boolean magAfspraakWijzigen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_MAMMA_AFSPRAAK_WIJZIGEN, Actie.INZIEN);
			verzetten.setVisible(magAfspraakWijzigen);
			inhoud.add(verzetten);

			AjaxLink<MammaAfspraak> afmelden = new IndicatingAjaxLink<MammaAfspraak>("afmelden", getModel())
			{
				@Override
				public void onClick(AjaxRequestTarget target)
				{
					afmelden(target, getModelObject());
				}

			};
			boolean magAfmelden = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_MAMMA_AFMELDEN, Actie.INZIEN);
			afmelden.setVisible(magAfmelden);
			inhoud.add(afmelden);

			AjaxLink<MammaAfspraak> maakBrief = new IndicatingAjaxLink<MammaAfspraak>("brief", getModel())
			{
				@Override
				public void onClick(AjaxRequestTarget target)
				{
					maakBrief(getModelObject());
					this.setVisible(false);
					target.add(inhoud);
				}
			};
			maakBrief.setVisible(magAfspraakWijzigen && afspraakService.magBevestigingsbriefAanmaken(getModelObject()));
			inhoud.add(maakBrief);

			AjaxLink<MammaAfspraak> uitstellen = new IndicatingAjaxLink<MammaAfspraak>("uitstellen", getModel())
			{
				@Override
				public void onClick(AjaxRequestTarget target)
				{
					uitstellen(target, getModelObject());
				}

			};
			uitstellen.setVisible(magAfspraakWijzigen && afspraakService.magUitstellen(client.getMammaDossier()));
			inhoud.add(uitstellen);
		}
		else
		{
			inhoud.add(new EmptyPanel("uitstellen").setVisible(false));
		}

	}

	public void maakBrief(MammaAfspraak afspraak)
	{
		baseBriefService.maakMammaBrief(afspraak.getUitnodiging().getScreeningRonde(), BriefType.MAMMA_AFSPRAAK_VERZET);
		info(getString("brief.aangemaakt"));
	}

	public abstract void verzetten(AjaxRequestTarget target, MammaAfspraak afspraak);

	public abstract void uitstellen(AjaxRequestTarget target, MammaAfspraak afspraak);

	public abstract void afmelden(AjaxRequestTarget target, MammaAfspraak afspraak);

	private class StandplaatsFragment extends Fragment
	{

		public StandplaatsFragment(String id, Date vanaf, MammaStandplaats standplaats)
		{
			super(id, "standplaatsFragment", MammaAfspraakPanel.this);

			MammaStandplaatsLocatie locatie = null;

			add(new Label("standplaats.naam", standplaats.getNaam()));
			MammaStandplaatsLocatie tijdelijkeLocatie = standplaats.getTijdelijkeLocatie();
			if (tijdelijkeLocatie.getStartDatum() != null)
			{
				Date eindDatum = tijdelijkeLocatie.getEindDatum();
				eindDatum.setHours(23);
				eindDatum.setMinutes(59);
				if (tijdelijkeLocatie.getStartDatum().compareTo(vanaf) * vanaf.compareTo(eindDatum) > 0)
				{
					locatie = tijdelijkeLocatie;
				}
			}
			if (locatie == null)
			{
				locatie = standplaats.getLocatie();
			}

			IModel<MammaStandplaatsLocatie> locatieModel = ModelUtil.cRModel(locatie);

			WebMarkupContainer locatieContainer = new WebMarkupContainer("locatieContainer", locatieModel);
			add(locatieContainer);
			locatieContainer.add(new Label("straat"));
			locatieContainer.add(new Label("huisnummer"));
			locatieContainer.add(new Label("postcode"));
			locatieContainer.add(new Label("plaats"));
			locatieContainer.add(new Label("locatieBeschrijving"));

			WebMarkupContainer tijdelijkContainer = new WebMarkupContainer("tijdelijkContainer");
			locatieContainer.add(tijdelijkContainer);
			tijdelijkContainer.setVisible(locatie.getTijdelijk());
			tijdelijkContainer.add(DateLabel.forDatePattern("startDatum", "dd-MM-yyyy"));
			tijdelijkContainer.add(DateLabel.forDatePattern("eindDatum", "dd-MM-yyyy"));
		}
	}
}
