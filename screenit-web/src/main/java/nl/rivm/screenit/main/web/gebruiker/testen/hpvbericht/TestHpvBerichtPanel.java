package nl.rivm.screenit.main.web.gebruiker.testen.hpvbericht;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.dao.cervix.CervixBMHKLaboratoriumDao;
import nl.rivm.screenit.main.service.cervix.HpvSendingMessageService;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.berichten.ScreenITResponseV251MessageWrapper;
import nl.rivm.screenit.util.cervix.HpvBerichtGenerator.CervixHpvBerichtGenerator;
import nl.rivm.screenit.util.cervix.HpvBerichtGenerator.CervixHpvBerichtGeneratorMonsterWrapper;
import nl.rivm.screenit.util.cervix.HpvBerichtGenerator.CervixHpvBerichtGeneratorWrapper;
import nl.rivm.screenit.util.cervix.HpvBerichtGenerator.CervixHpvBerichtWaarde;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.util.StringUtil;

public class TestHpvBerichtPanel extends GenericPanel<CervixHpvBerichtGeneratorWrapper>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private CervixBMHKLaboratoriumDao cervixBMHKLaboratoriumDao;

	@SpringBean
	private HpvSendingMessageService hpvSendingMessageService;

	WebMarkupContainer uitslagenContainer = null;

	public TestHpvBerichtPanel(String id)
	{
		super(id, new CompoundPropertyModel<CervixHpvBerichtGeneratorWrapper>(new CervixHpvBerichtGeneratorWrapper()));
		CervixHpvBerichtGeneratorWrapper wrapper = getModelObject();
		wrapper.getMonsterWrappers().add(new CervixHpvBerichtGeneratorMonsterWrapper());

		Form<CervixHpvBerichtGeneratorWrapper> form = new Form<CervixHpvBerichtGeneratorWrapper>("form", getModel());
		add(form);

		ComponentHelper.addTextField(form, "messageId", true, 100, false);
		ComponentHelper.addTextField(form, "instrumentId", true, 100, false);
		ComponentHelper.addTextField(form, "labNaam", true, 100, false);

		uitslagenContainer = getUitslagenContainer(wrapper);
		form.add(uitslagenContainer);

		form.add(new IndicatingAjaxSubmitLink("verstuurHl7Bericht", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				CervixHpvBerichtGeneratorWrapper wrapper = getModelObject();
				String alAangeleverd = "";
				if (cervixBMHKLaboratoriumDao.isHpvBerichtAlOntvangen(wrapper.getMessageId()))
				{

					alAangeleverd = ", Bericht al een keer aangeleverd met dit messageId. Dit bericht wordt daarom niet meer verwerkt door de batch maar levert wel een AA op!";
				}

				Message hl7bericht = CervixHpvBerichtGenerator.geefHl7Bericht(wrapper);

				ScreenITResponseV251MessageWrapper result = hpvSendingMessageService.verstuurHpvBericht(hl7bericht);

				String melding = "Bericht succesvol verstuurd, Code: ";
				if (result != null)
				{
					melding += result.getAcknowledgmentCode();
					if (result.getMelding() != null)
					{
						melding += ", reden: " + result.getMelding();
					}
				}
				else
				{
					melding += "<geen response>";
				}
				if (StringUtil.isNotBlank(alAangeleverd))
				{
					melding += alAangeleverd;
				}
				info(melding);

			}
		});

		form.add(new IndicatingAjaxLink<CervixHpvBerichtGeneratorWrapper>("uitslagToevoegen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				CervixHpvBerichtGeneratorMonsterWrapper monsterWrapper = new CervixHpvBerichtGeneratorMonsterWrapper();
				TestHpvBerichtPanel.this.getModelObject().getMonsterWrappers().add(monsterWrapper);
				target.add(uitslagenContainer);
			}
		});
	}

	private WebMarkupContainer getUitslagenContainer(CervixHpvBerichtGeneratorWrapper wrapper)
	{
		WebMarkupContainer container = new WebMarkupContainer("uitslagenContainer");
		container.setOutputMarkupId(true);

		PropertyListView<CervixHpvBerichtGeneratorMonsterWrapper> list = new PropertyListView<CervixHpvBerichtGeneratorMonsterWrapper>("monsterWrappers",
			new PropertyModel<List<CervixHpvBerichtGeneratorMonsterWrapper>>(getModel(), "monsterWrappers"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(final ListItem<CervixHpvBerichtGeneratorMonsterWrapper> item)
			{
				item.add(ComponentHelper.addTextField(this, "barcode", true, 100, false));
				ListModel<CervixHpvBerichtWaarde> uitslagen = new ListModel<CervixHpvBerichtWaarde>(Arrays.asList(CervixHpvBerichtWaarde.values()));
				item.add(new DropDownChoice<CervixHpvBerichtWaarde>("uitslag", uitslagen, new EnumChoiceRenderer<CervixHpvBerichtWaarde>()));

				item.add(ComponentHelper.monthYearDatePicker("autorisatieDatum").setRequired(true));
				item.add(ComponentHelper.monthYearDatePicker("analyseDatum").setRequired(true));

				item.add(new IndicatingAjaxLink<CervixHpvBerichtGeneratorWrapper>("verwijderen")
				{

					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						CervixHpvBerichtGeneratorMonsterWrapper monsterWrapper = item.getModelObject();

						List<CervixHpvBerichtGeneratorMonsterWrapper> wrappers = TestHpvBerichtPanel.this.getModelObject().getMonsterWrappers();
						if (wrappers.contains(monsterWrapper))
						{
							wrappers.remove(monsterWrapper);
						}
						target.add(uitslagenContainer);
					}
				});
			}

		};
		container.add(list);
		return container;

	}

}
