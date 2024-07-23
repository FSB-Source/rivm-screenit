package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.bmhklaboratorium;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.cervix.CervixBetalingService;
import nl.rivm.screenit.main.service.cervix.CervixVerrichtingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.form.BigDecimalField;
import nl.rivm.screenit.main.web.component.validator.BigDecimalPuntFormValidator;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.cervix.CervixHerindexeringWaarschuwingPanel;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.rivm.screenit.util.cervix.CervixTariefUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.hibernate.Hibernate;

public abstract class CervixLaboratoriumIndexerenPopupPanel extends GenericPanel<CervixLabTarief>
{
	@SpringBean
	private CervixVerrichtingService verrichtingService;

	@SpringBean
	private CervixBetalingService betalingService;

	public CervixLaboratoriumIndexerenPopupPanel(String id)
	{
		super(id, ModelUtil.ccModel(new CervixLabTarief()));

		var instelling = (BMHKLaboratorium) ScreenitSession.get().getCurrentSelectedOrganisatie();
		var tarief = getModelObject();
		tarief.setBmhkLaboratorium(instelling);

		var latest = verrichtingService.getLatestLabTarief(instelling);
		CervixTariefUtil.vulTarief(tarief, latest);

		var organisatie = ScreenitSession.get().getCurrentSelectedOrganisatie();

		var form = new Form<>("form", getModel());

		form.add(new CervixHerindexeringWaarschuwingPanel("waarschuwing"));

		form.add(new ListView<>("tariefTypen", betalingService.getTariefTypenVoorLaboratorium((BMHKLaboratorium) Hibernate.unproxy(organisatie)))
		{
			@Override
			protected void populateItem(ListItem<CervixTariefType> listItem)
			{
				var tariefType = listItem.getModelObject();

				var label = new EnumLabel<>("label", tariefType);
				listItem.add(label);

				var field = new BigDecimalField("tarief");
				field.setModel(new PropertyModel<>(form.getModel(), tariefType.getBedragProperty()));
				field.setRequired(true);
				form.add(field);
				form.add(new BigDecimalPuntFormValidator(field));
				listItem.add(field);
			}
		});

		var startDatumDatePicker = ComponentHelper.newDatePicker("geldigVanafDatum");
		startDatumDatePicker.setRequired(true);
		startDatumDatePicker.setLabel(Model.of("Geldig van"));
		form.add(startDatumDatePicker);

		var eindDatumDatePicker = ComponentHelper.newDatePicker("geldigTotenmetDatum");
		eindDatumDatePicker.setLabel(Model.of("Geldig t/m"));
		form.add(eindDatumDatePicker);

		form.add(new DependantDateValidator(startDatumDatePicker, eindDatumDatePicker, DependantDateValidator.Operator.AFTER));

		form.add(new IndicatingAjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				try
				{
					var nieuweTarief = (CervixTarief) HibernateHelper.deproxy(ModelProxyHelper.deproxy(form.getModelObject()));
					var melding = betalingService.toevoegenIndexatieTarief(nieuweTarief, ScreenitSession.get().getLoggedInAccount());
					opslaan(target, melding);
				}
				catch (IllegalArgumentException e)
				{
					error(getString(e.getMessage()));
				}
			}
		});

		add(form);

	}

	protected abstract void opslaan(AjaxRequestTarget target, String melding);

}
