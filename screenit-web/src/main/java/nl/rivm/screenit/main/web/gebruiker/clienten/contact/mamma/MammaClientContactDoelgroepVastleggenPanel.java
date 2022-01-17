package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaClientContactDoelgroepVastleggenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	private MammaDoelgroep oldDoelgroep;

	private IModel<Client> client;

	private TextField<String> dubbeleTijdReden;

	private WebMarkupContainer dubbeleTijdRedenContainer;

	public MammaClientContactDoelgroepVastleggenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);
		this.client = client;

		oldDoelgroep = client.getObject().getMammaDossier().getDoelgroep();
		WebMarkupContainer doelgroepContainer = new WebMarkupContainer("doelgroepContainer", new CompoundPropertyModel<>(client));

		ScreenitDropdown dropdown = new ScreenitDropdown<MammaDoelgroep>("mammaDossier.doelgroep", Arrays.asList(MammaDoelgroep.values()),
			new EnumChoiceRenderer<MammaDoelgroep>());
		add(doelgroepContainer);
		dropdown.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
			{
				refreshRedenField(ajaxRequestTarget);
			}

			private void refreshRedenField(AjaxRequestTarget target)
			{
				dubbeleTijdRedenContainer.setVisible(dropdown.getModel().getObject().equals(MammaDoelgroep.DUBBELE_TIJD));
				target.add(doelgroepContainer);
			}

		});
		doelgroepContainer.setOutputMarkupId(true);
		doelgroepContainer.add(dropdown);

		boolean hadAlDubbeleTijdDoelgroep = client.getObject().getMammaDossier().getDoelgroep().equals(MammaDoelgroep.DUBBELE_TIJD);
		dubbeleTijdRedenContainer = new WebMarkupContainer("dubbeleTijdRedenContainer", new CompoundPropertyModel<>(client));
		dubbeleTijdReden = ComponentHelper.newTextField("mammaDossier.dubbeleTijdReden", 255, true);
		dubbeleTijdRedenContainer.setOutputMarkupId(true);
		dubbeleTijdRedenContainer.setVisible(hadAlDubbeleTijdDoelgroep);
		dubbeleTijdRedenContainer.add(dubbeleTijdReden);
		doelgroepContainer.add(dubbeleTijdRedenContainer);
	}

	@Override
	public void validate()
	{
		super.validate();
		MammaDossier dossier = client.getObject().getMammaDossier();

		if (dossier.getDoelgroep().equals(oldDoelgroep))
		{
			error(getString("doelgroep.niet.gewijzigd"));
		}

		if (dossier.getTehuis() == null)
		{
			MammaScreeningRonde screeningRonde = dossier.getLaatsteScreeningRonde();

			if (screeningRonde != null)
			{
				MammaAfspraak laatsteAfspraak = MammaScreeningRondeUtil.getLaatsteAfspraak(screeningRonde);
				if (laatsteAfspraak != null && laatsteAfspraak.getStatus().equals(MammaAfspraakStatus.GEPLAND) && laatsteAfspraak.getVanaf().compareTo(dateSupplier.getDate()) >= 0)
				{
					ScreenitSession.get().warn(getString("client.heeft.afspraak"));
				}
			}
		}
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(client);
	}

}
