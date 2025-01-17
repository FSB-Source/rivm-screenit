package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.bmhklaboratorium;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.service.cervix.CervixBetalingService;
import nl.rivm.screenit.main.service.cervix.CervixVerrichtingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.form.BigDecimalField;
import nl.rivm.screenit.main.web.component.validator.BigDecimalPuntFormValidator;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixTariefUtil;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;
import org.hibernate.Hibernate;

public abstract class CervixLaboratoriumTarievenPopupPanel extends GenericPanel<CervixLabTarief>
{
	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private CervixVerrichtingService verrichtingService;

	@SpringBean
	private CervixBetalingService betalingService;

	public CervixLaboratoriumTarievenPopupPanel(String id)
	{
		super(id, ModelUtil.ccModel(new CervixLabTarief()));

		var instelling = (BMHKLaboratorium) ScreenitSession.get().getCurrentSelectedOrganisatie();
		var tarief = getModelObject();
		tarief.setBmhkLaboratorium(instelling);

		var latest = verrichtingService.getLatestLabTarief(instelling);
		CervixTariefUtil.vulTarief(tarief, latest);

		var organisatie = ScreenitSession.get().getCurrentSelectedOrganisatie();

		var form = new Form<>("form", getModel());

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
		startDatumDatePicker.add(DateValidator.minimum(DateUtil.plusDagen(currentDateSupplier.getDateMidnight(), 1)));
		startDatumDatePicker.setLabel(Model.of("Geldig vanaf"));
		form.add(startDatumDatePicker);

		form.add(new AjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var tarief = ModelProxyHelper.deproxy(form.getModelObject());
				if (verrichtingService.heeftLaboratoriumTarief(tarief))
				{
					error("Er is al een tarief met dezelfde geldig vanaf datum, selecteer een andere datum.");
				}
				else
				{
					betalingService.toevoegenTarief(tarief, ScreenitSession.get().getLoggedInAccount());
					opslaan(target);
				}
			}
		});

		add(form);

	}

	protected abstract void opslaan(AjaxRequestTarget target);

}
