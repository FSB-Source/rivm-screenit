package nl.rivm.screenit.main.web.gebruiker.screening.mamma.afspraken;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientContactActieTypeWrapper;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.MammaDoelgroepIndicatorPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.formatter.TelefoonnummersFormatter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.HibernateCheckBoxListContainer;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class MammaAfsprakenBlokPanel extends GenericPanel<List<MammaAfspraak>>
{

	private final HibernateCheckBoxListContainer<MammaAfspraak> selectedAfspraken;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	private boolean magBulkVerzetten;

	public MammaAfsprakenBlokPanel(String id, IModel<List<MammaAfspraak>> afsprakenModel, HibernateCheckBoxListContainer<MammaAfspraak> selectedAfspraken, LocalDate currentDay,
		boolean magVerzetten, boolean magBulkVerzetten)
	{
		super(id, afsprakenModel);
		this.selectedAfspraken = selectedAfspraken;
		this.magBulkVerzetten = magBulkVerzetten;

		setOutputMarkupId(true);

		WebMarkupContainer selectColumn = new WebMarkupContainer("selectColumn");
		selectColumn.setVisible(magBulkVerzetten);
		add(selectColumn);

		add(new ListView<MammaAfspraak>("items", afsprakenModel)
		{
			@Override
			protected void populateItem(ListItem<MammaAfspraak> item)
			{
				MammaAfspraak afspraak = item.getModelObject();
				MammaScreeningRonde laatsteScreeningRonde = afspraak.getUitnodiging().getScreeningRonde().getDossier().getLaatsteScreeningRonde();

				addCheckbox(item);
				item.add(DateLabel.forDatePattern("tijd", Model.of(afspraak.getVanaf()), "HH:mm"));
				Client client = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();
				item.add(new Label("client", NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(client)));
				String datePattern = Constants.DEFAULT_DATE_FORMAT;
				if (client.getPersoon().getGeboortedatumPrecisie() != null)
				{
					datePattern = client.getPersoon().getGeboortedatumPrecisie().getDatePattern();
				}
				item.add(DateLabel.forDatePattern("geboortedatum", Model.of(client.getPersoon().getGeboortedatum()), datePattern));
				item.add(new Label("bsn", client.getPersoon().getBsn()));
				item.add(new Label("telefoonnr", TelefoonnummersFormatter.getTelefoonnummersVoorPersoon(client.getPersoon())));
				item.add(new Label("opkomstkans", BigDecimalUtil.decimalToString(afspraak.getOpkomstkans().getOpkomstkans().multiply(BigDecimal.valueOf(100)), 0) + "%"));
				item.add(new MammaDoelgroepIndicatorPanel("doelgroep", client.getMammaDossier(), true));

				String verzet = "Nee";
				if (afspraak.getVerzettenReden() != null)
				{
					verzet = "Ja (" + getString(EnumStringUtil.getPropertyString(afspraak.getVerzettenReden())) + ")";
				}
				item.add(new Label("verzet", verzet));

				AjaxLink<Void> verzetten = new AjaxLink<Void>("verzetten")
				{
					@Override
					public void onClick(AjaxRequestTarget target)
					{
						MammaAfspraak afspraak = (MammaAfspraak) HibernateHelper.deproxy(ModelProxyHelper.deproxy(item.getModelObject()));
						List<Object> extraParameters = new ArrayList<>();
						extraParameters.add(afspraak);
						extraParameters.add(MammaAfspraakStatus.GEPLAND);
						extraParameters.add(getPage().getDefaultModelObject());
						extraParameters.add(Constants.CONTACT_EXTRA_PARAMETER_VANUIT_BK_PLANNING);
						extraParameters.add(currentDay);

						ClientContactActieTypeWrapper clientContactActieTypeWrapper = afspraak.getVanaf().compareTo(dateSupplier.getDate()) < 0
							? ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN
							: ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN;

						setResponsePage(new MammaAfspraakVerzettenAfmeldenPage(ModelUtil.sModel(afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient()),
							extraParameters, clientContactActieTypeWrapper));
					}
				};
				MammaAfspraak laatsteAfspraak = laatsteScreeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak();
				verzetten.setVisible(magVerzetten && laatsteAfspraak != null && laatsteAfspraak.equals(afspraak)
					&& !AfmeldingUtil.isEenmaligOfDefinitefAfgemeld(laatsteAfspraak.getUitnodiging().getScreeningRonde().getDossier()));
				item.add(verzetten);
			}
		});
	}

	private void addCheckbox(ListItem<MammaAfspraak> item)
	{
		MammaAfspraak afspraak = item.getModelObject();
		MammaDossier dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
		GbaPersoon persoon = dossier.getClient().getPersoon();

		MammaScreeningRonde laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();
		MammaAfspraak laatsteAfspraak = laatsteScreeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak();

		CheckBox select = new CheckBox("select", new PropertyModel<>(selectedAfspraken.getValueMap(), item.getModelObject().getId().toString()));
		select.setVisible(
			magBulkVerzetten
				&& afspraak.getStatus().equals(MammaAfspraakStatus.GEPLAND)
				&& laatsteAfspraak.equals(afspraak)
				&& dossier.getTehuis() == null
				&& !dossier.getDoelgroep().equals(MammaDoelgroep.MINDER_VALIDE)
				&& !AfmeldingUtil.isEenmaligOfDefinitefAfgemeld(dossier)
				&& !MammaScreeningRondeUtil.heeftActiefUitstel(laatsteScreeningRonde)
				&& persoon.getOverlijdensdatum() == null
				&& persoon.getDatumVertrokkenUitNederland() == null
				&& !persoon.getGbaAdres().getGbaGemeente().getCode().equals(Gemeente.RNI_CODE)
		);
		item.add(select);
		selectedAfspraken.addObject(afspraak);
	}
}
