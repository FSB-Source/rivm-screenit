package nl.rivm.screenit.main.web.gebruiker.testen.colon;

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

import nl.rivm.screenit.main.web.component.ScreenitDateTextField;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.testen.TestenBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.TestModel;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.ColonTest;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.colon.ColonTestService;
import nl.rivm.screenit.service.colon.ColonTestStateService;
import nl.rivm.screenit.util.TestBsnGenerator;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.enumdropdownchoice.EnumChoiceRenderer;
import nl.topicuszorg.wicket.input.enumdropdownchoice.EnumDropDownChoice;
import nl.topicuszorg.wicket.input.validator.BSNValidator;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.Radio;
import org.apache.wicket.markup.html.form.RadioGroup;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.TESTEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
public class ColonTestPage extends TestenBasePage
{
	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ColonTestStateService testStateService;

	@SpringBean
	private ColonTestService colonTestervice;

	private IModel<TestModel> testModel;

	private static final long serialVersionUID = 1L;

	@SuppressWarnings("unchecked")
	public ColonTestPage()
	{
		if (testModel == null)
		{
			TestModel test = new TestModel();
			test.setGeslacht(Geslacht.MAN);
			testModel = new CompoundPropertyModel<TestModel>(test);
		}

		final Form<TestModel> form = new Form<TestModel>("testForm", getTestModel());
		final TextField<String> bsnField = new TextField<String>("bsn");
		bsnField.add(new BSNValidator(true, true));
		bsnField.setRequired(true);
		bsnField.setOutputMarkupId(true);
		form.add(bsnField);
		form.add(new IndicatingAjaxLink<TestModel>("bsnGenereren", getTestModel())
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				TestModel object = getModelObject();
				object.setBsn(TestBsnGenerator.getValideBsn());
				target.add(bsnField);
			}
		});
		form.add(new ScreenitDateTextField("geboortedatum").setOutputMarkupId(true).add(new AjaxFormComponentUpdatingBehavior("change")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(getComponent());
			}
		}));
		form.add(new ScreenitDropdown<Geslacht>("geslacht", Arrays.asList(Geslacht.values()), new EnumChoiceRenderer<Geslacht>()));
		form.add(new ScreenitDateTextField("datumOverlijden").setOutputMarkupId(true).add(new AjaxFormComponentUpdatingBehavior("change")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(getComponent());
			}
		}));

		form.add(new DropDownChoice<Gemeente>("gemeente",
			ModelUtil.listRModel(
				hibernateService.getHibernateSession().createCriteria(Gemeente.class).add(Restrictions.isNotNull("screeningOrganisatie")).addOrder(Order.asc("naam")).list(),
				false),
			new ChoiceRenderer<Gemeente>("naam")));

		form.add(new EnumDropDownChoice<>("gbaStatus", GbaStatus.class, false).setOutputMarkupId(true));

		final RadioGroup<ColonTest> testGroup = new RadioGroup<ColonTest>("colonTestActies");
		List<ColonTest> availableColonTestActies = Arrays.asList(ColonTest.values());
		testGroup.add(new ListView<ColonTest>("testactie", availableColonTestActies)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<ColonTest> item)
			{
				Radio<ColonTest> radio = new Radio<ColonTest>("radio", new Model<ColonTest>(item.getModelObject()));
				item.add(radio);
				item.add(new EnumLabel<ColonTest>("label", item.getModelObject()));
				item.setVisible(item.getModelObject().getActief());
			}
		});
		form.add(testGroup);
		add(form);

		IndicatingAjaxSubmitLink submit = new IndicatingAjaxSubmitLink("submit", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				String feedback = testStateService.setClientInState(getTestModel().getObject());
				ColonTest colonTest = getTestModel().getObject().getColonTestActies();
				if (colonTest == null)
				{
					error(feedback);
				}
				else
				{
					info(feedback);
				}
			}

		};

		form.add(submit);

		maakMarkeerNietVerstuurdeUitnodigingenAlsVerstuurd();
	}

	private void maakMarkeerNietVerstuurdeUitnodigingenAlsVerstuurd()
	{
		final Model<String> aantalModel = Model.of("");
		final Label aantalLabel = new Label("aantalGemarkeerdeUitnodigingen", aantalModel);
		aantalLabel.setOutputMarkupId(true);
		add(aantalLabel);

		add(new IndicatingAjaxLink<Void>("uitnodigingenDoorzetten")
		{
			private static final long serialVersionUID1 = 1L;

			@Override
			public void onClick(AjaxRequestTarget ajaxRequestTarget)
			{
				final int aantal = colonTestervice.markeerNogNietNaarInpakcentrumVerstuurdeUitnodigingenAlsVerstuurd();
				aantalModel.setObject("Aantal als verstuurd gemarkeerde uitnodigingen: " + aantal);
				ajaxRequestTarget.add(aantalLabel);
			}
		});
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(getTestModel());
	}

	public IModel<TestModel> getTestModel()
	{
		return testModel;
	}

	public void setTestModel(IModel<TestModel> testModel)
	{
		this.testModel = testModel;
	}
}
